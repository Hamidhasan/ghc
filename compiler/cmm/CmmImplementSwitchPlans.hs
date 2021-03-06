{-# LANGUAGE GADTs #-}
module CmmImplementSwitchPlans
  ( cmmImplementSwitchPlans
  )
where

import Hoopl
import BlockId
import Cmm
import CmmUtils
import CmmSwitch
import UniqSupply
import DynFlags

--
-- This module replaces Switch statements as generated by the Stg -> Cmm
-- transformation, which might be huge and sparse and hence unsuitable for
-- assembly code, by proper constructs (if-then-else trees, dense jump tables).
--
-- The actual, abstract strategy is determined by createSwitchPlan in
-- CmmSwitch and returned as a SwitchPlan; here is just the implementation in
-- terms of Cmm code. See Note [Cmm Switches, the general plan] in CmmSwitch.
--
-- This division into different modules is both to clearly separte concerns,
-- but also because createSwitchPlan needs access to the constructors of
-- SwitchTargets, a data type exported abstractly by CmmSwitch.
--

-- | Traverses the 'CmmGraph', making sure that 'CmmSwitch' are suitable for
-- code generation.
cmmImplementSwitchPlans :: DynFlags -> CmmGraph -> UniqSM CmmGraph
cmmImplementSwitchPlans dflags g
    | targetSupportsSwitch (hscTarget dflags) = return g
    | otherwise = do
    blocks' <- concat `fmap` mapM (visitSwitches dflags) (toBlockList g)
    return $ ofBlockList (g_entry g) blocks'

visitSwitches :: DynFlags -> CmmBlock -> UniqSM [CmmBlock]
visitSwitches dflags block
  | (entry@(CmmEntry _ scope), middle, CmmSwitch expr ids) <- blockSplit block
  = do
    let plan = createSwitchPlan ids

    (newTail, newBlocks) <- implementSwitchPlan dflags scope expr plan

    let block' = entry `blockJoinHead` middle `blockAppend` newTail

    return $ block' : newBlocks

  | otherwise
  = return [block]


-- Implementing a switch plan (returning a tail block)
implementSwitchPlan :: DynFlags -> CmmTickScope -> CmmExpr -> SwitchPlan -> UniqSM (Block CmmNode O C, [CmmBlock])
implementSwitchPlan dflags scope expr = go
  where
    go (Unconditionally l)
      = return (emptyBlock `blockJoinTail` CmmBranch l, [])
    go (JumpTable ids)
      = return (emptyBlock `blockJoinTail` CmmSwitch expr ids, [])
    go (IfLT signed i ids1 ids2)
      = do
        (bid1, newBlocks1) <- go' ids1
        (bid2, newBlocks2) <- go' ids2

        let lt | signed    = cmmSLtWord
               | otherwise = cmmULtWord
            scrut = lt dflags expr $ CmmLit $ mkWordCLit dflags i
            lastNode = CmmCondBranch scrut bid1 bid2
            lastBlock = emptyBlock `blockJoinTail` lastNode
        return (lastBlock, newBlocks1++newBlocks2)
    go (IfEqual i l ids2)
      = do
        (bid2, newBlocks2) <- go' ids2

        let scrut = cmmNeWord dflags expr $ CmmLit $ mkWordCLit dflags i
            lastNode = CmmCondBranch scrut bid2 l
            lastBlock = emptyBlock `blockJoinTail` lastNode
        return (lastBlock, newBlocks2)

    -- Same but returning a label to branch to
    go' (Unconditionally l)
      = return (l, [])
    go' p
      = do
        bid <- mkBlockId `fmap` getUniqueM
        (last, newBlocks) <- go p
        let block = CmmEntry bid scope `blockJoinHead` last
        return (bid, block: newBlocks)
