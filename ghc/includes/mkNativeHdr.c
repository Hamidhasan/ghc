/* --------------------------------------------------------------------------
 * $Id: mkNativeHdr.c,v 1.8 2002/01/08 10:36:24 sewardj Exp $
 *
 * (c) The GHC Team, 1992-1998
 *
 * Generate a header for the native code generator
 *
 * ------------------------------------------------------------------------*/

#include "Stg.h"

#define OFFSET(table, x) ((StgUnion *) &(x) - (StgUnion *) (&table))

#define OFFSET_R1    OFFSET(RegTable, RegTable.rR1)
#define OFFSET_R2    OFFSET(RegTable, RegTable.rR2)
#define OFFSET_R3    OFFSET(RegTable, RegTable.rR3)
#define OFFSET_R4    OFFSET(RegTable, RegTable.rR4)
#define OFFSET_R5    OFFSET(RegTable, RegTable.rR5)
#define OFFSET_R6    OFFSET(RegTable, RegTable.rR6)
#define OFFSET_R7    OFFSET(RegTable, RegTable.rR7)
#define OFFSET_R8    OFFSET(RegTable, RegTable.rR8)
#define OFFSET_R9    OFFSET(RegTable, RegTable.rR9)
#define OFFSET_R10   OFFSET(RegTable, RegTable.rR10)
#define OFFSET_F1    OFFSET(RegTable, RegTable.rF1)
#define OFFSET_F2    OFFSET(RegTable, RegTable.rF2)
#define OFFSET_F3    OFFSET(RegTable, RegTable.rF3)
#define OFFSET_F4    OFFSET(RegTable, RegTable.rF4)
#define OFFSET_D1    OFFSET(RegTable, RegTable.rD1)
#define OFFSET_D2    OFFSET(RegTable, RegTable.rD2)
#define OFFSET_L1    OFFSET(RegTable, RegTable.rL1)
#define OFFSET_Sp    OFFSET(RegTable, RegTable.rSp)
#define OFFSET_Su    OFFSET(RegTable, RegTable.rSu)
#define OFFSET_SpLim OFFSET(RegTable, RegTable.rSpLim)
#define OFFSET_Hp    OFFSET(RegTable, RegTable.rHp)
#define OFFSET_HpLim OFFSET(RegTable, RegTable.rHpLim)
#define OFFSET_CurrentTSO OFFSET(RegTable, RegTable.rCurrentTSO)
#define OFFSET_CurrentNursery OFFSET(RegTable, RegTable.rCurrentNursery)
#define OFFSET_HpAlloc OFFSET(RegTable, RegTable.rHpAlloc)

#define FUN_OFFSET(sym) ((StgPtr)&cap.f.sym - (StgPtr)&cap.r)

#define OFFSET_stgChk0       FUN_OFFSET(stgChk0)
#define OFFSET_stgChk1       FUN_OFFSET(stgChk1)
#define OFFSET_stgGCEnter1   FUN_OFFSET(stgGCEnter1)
#define OFFSET_stgUpdatePAP  FUN_OFFSET(stgUpdatePAP)

#define OFFW_Capability_r  OFFSET(cap, cap.r)

#define TSO_SP       OFFSET(tso, tso.sp)
#define TSO_SU       OFFSET(tso, tso.su)
#define TSO_STACK    OFFSET(tso, tso.stack)

#define BDESCR_START OFFSET(bd, bd.start)
#define BDESCR_FREE  OFFSET(bd, bd.free)
#define BDESCR_BLOCKS OFFSET(bd, bd.blocks)

StgRegTable RegTable;

Capability cap;

StgTSO tso;
bdescr bd;

int
main()
{
    printf("-- This file is created automatically.  Do not edit by hand.\n\n");

    printf("\n-- Base table offsets for the Native Code Generator\n");

    printf("#define OFFSET_R1 %d\n", OFFSET_R1);
    printf("#define OFFSET_R2 %d\n", OFFSET_R2);
    printf("#define OFFSET_R3 %d\n", OFFSET_R3);
    printf("#define OFFSET_R4 %d\n", OFFSET_R4);
    printf("#define OFFSET_R5 %d\n", OFFSET_R5);
    printf("#define OFFSET_R6 %d\n", OFFSET_R6);
    printf("#define OFFSET_R7 %d\n", OFFSET_R7);
    printf("#define OFFSET_R8 %d\n", OFFSET_R8);
    printf("#define OFFSET_R9 %d\n", OFFSET_R9);
    printf("#define OFFSET_R10 %d\n", OFFSET_R10);
    printf("#define OFFSET_F1 %d\n", OFFSET_F1);
    printf("#define OFFSET_F2 %d\n", OFFSET_F2);
    printf("#define OFFSET_F3 %d\n", OFFSET_F3);
    printf("#define OFFSET_F4 %d\n", OFFSET_F4);
    printf("#define OFFSET_D1 %d\n", OFFSET_D1);
    printf("#define OFFSET_D2 %d\n", OFFSET_D2);
#ifdef SUPPORT_LONG_LONGS
    printf("#define OFFSET_L1 %d\n", OFFSET_L1);
#endif
    printf("#define OFFSET_Sp %d\n", OFFSET_Sp);
    printf("#define OFFSET_Su %d\n", OFFSET_Su);
    printf("#define OFFSET_SpLim %d\n", OFFSET_SpLim);
    printf("#define OFFSET_Hp %d\n", OFFSET_Hp);
    printf("#define OFFSET_HpLim %d\n", OFFSET_HpLim);
    printf("#define OFFSET_CurrentTSO %d\n", OFFSET_CurrentTSO);
    printf("#define OFFSET_CurrentNursery %d\n", OFFSET_CurrentNursery);
    printf("#define OFFSET_HpAlloc %d\n", OFFSET_HpAlloc);

    printf("#define OFFSET_stgChk0 (%d)\n", OFFSET_stgChk0);
    printf("#define OFFSET_stgChk1 (%d)\n", OFFSET_stgChk1);
    printf("#define OFFSET_stgGCEnter1 (%d)\n", OFFSET_stgGCEnter1);
    printf("#define OFFSET_stgUpdatePAP (%d)\n", OFFSET_stgUpdatePAP);

    printf("\n-- Offset of the .r (StgRegTable) field in a Capability\n");

    printf("#define OFFW_Capability_r (%d)\n", OFFW_Capability_r);

    printf("\n-- Storage Manager offsets for the Native Code Generator\n");

    printf("\n-- TSO offsets for the Native Code Generator\n");

    printf("#define TSO_SP %d\n", TSO_SP);
    printf("#define TSO_SU %d\n", TSO_SU);
    printf("#define TSO_STACK %d\n", TSO_STACK);

    printf("\n-- Block descriptor offsets for the Native Code Generator\n");

    printf("#define BDESCR_START %d\n", BDESCR_START);
    printf("#define BDESCR_FREE %d\n", BDESCR_FREE);
    printf("#define BDESCR_BLOCKS %d\n", BDESCR_BLOCKS);

    printf("\n-- FILE size for the Native Code Generator\n");

    printf("#define FILE_SIZE %d\n", sizeof(*stdin));

    exit(0);
}
