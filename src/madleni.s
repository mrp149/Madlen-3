C+HISTORY   08/08/81  17.27.15
C+HISTORY   28/07/81  12.06.06
C+HISTORY   23/07/81  10.24.31
C+HISTORY   21/07/81  19.05.24
C+HISTORY   21/07/81  12.31.44
C+HISTORY   21/07/81  07.36.49
C+HISTORY   18/07/81  17.43.58
C+HISTORY   17/07/81  15.42.11
C+HISTORY   09/07/81  07.17.14
C+HISTORY   04/07/81  19.15.31
C+HISTORY   26/06/81  14.33.08
C+HISTORY   18/06/81  08.07.57
C+HISTORY   04/06/81  16.26.17
C+HISTORY   02/06/81  07.57.21
C
C+HISTORY   01/06/81  05.25.17
C+HISTORY   30/05/81  13.35.59
C+HISTORY   29/05/81  01.35.10
C+HISTORY   27/05/81  06.05.38
 MADLENI*:,NAME,
 ,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ГEHEPAЦИЯ AБCOЛЮTHOГO MOДYЛЯ.
C
C
C**************************************
C*                                    *
C*    ИHИЦИATOP ACCEMБЛEPA  MADLEN    *
C*                                    *
C* BEPCИЯ 1: M.Ю.ПOПOB     18/12/76   *
C* BEPCИЯ 2: E.D.ФEДЮHЬKИH 22/12/77   *
C* BEPCИЯ 3: M.Ю.ПOПOB, E.Д.ФEДЮHЬKИH *
C*                         21/08/79   *
C*                                    *
C* OБ'EДИHEHHЫЙ ИHCTИTYT ЯДEPHЫX      *
C* ИCCЛEДOBAHИЙ (ДYБHA).              *
C*                                    *
C**************************************
C
C
C**************************************
C*                                    *
C*      THE ROAD TO HELL IS PAVED     *
C*        WITH GOOD INTENSIONS        *
C*                                    *
C**************************************
C
C
 MONITOR*:,SUBP,
 ENTRY*:LC,BLOCK,PASS,INFSYS,INPUT,PAGE
 ,CONT,MOD,DD,EDIT
 ,CONT,LIST,XTEXT,SNAME(8)
 MONCARD*:,LC,14
 FULISFL*:,LC,1
 NOLISFL*:,LC,1
 *REGIM//:,LC,1
 PUNCHFL*:,LC,1
  ARREAD*:,LC,
  KCOUNT*:,LC,
C+   ИHФOPMAЦИOHHOE CЛOBO ДЛЯ Э70
C+   (ЧTEHИE ЛИCTA C HAЧAЛOM TEЛA
C+   TPAHCЛЯTOPA):
 /VERE/:LC,BLOCK,(1),RA
C
 W16:,EQU,16B
 W17:,EQU,17B
C
 STROKA:,EQU,INFOR+10
 LISTECK:,EQU,STROKA+310
 SIGNS:,EQU,STROKA+302
 LISTS:PC,BLOCK,SECTOR(4096),TABDEC(1024)
 NSEC:,P/P,(SECTOR)16
 NDEC:,P/P,(TABDEC)16
 NINF:,P/P,(INFOR)16
 NABEG:,P/P,(MSTACK)1024
 NAEND:,P/P,(ENDPTR*)1024
 NASEC:,P/P,(SECTOR)1024
 NADEC:,P/P,(TABDEC)1024
 NAINF:,P/P,(INFOR)1024
 DUMMY1:,P/P,(MADLEN*+4000B)1024
 PUNADR:,P*P,(DUMMY1)1024
C
C*    HOMEP ПEPBOГO ЛИCTA TEЛA TPAHC.
C
 NLASS:,P/P,(MSTACK)16
   MONREAD*:,SUBP,
  READ*:,SUBP,
  SAVE*:,SUBP,
 PRINT8**:,SUBP,
 FOMA***:,SUBP,
 PRINT8:,SUBP,
 COSYTISO:,SUBP,
 DRUMTAP*:,SUBP,
 STOP*   :,SUBP,
C........ COMMON BLOCKS .....
 SPASAN*:,LC,220
 ERRORBUF:,LC,13
 CHKWORD*:,LC,1
C........ RESTORE MEMORY ....
 ENDPTR*:,LC,0
 MDL*REF:,SUBP,
C
C
C*    BXOДЫ B БЛOK MADLEN*
C
 MADLEN*3:,SUBP,
 MADLEN*:,EQU,MADLEN*3
  SPY:,EQU,MADLEN*+1
 RUN:,EQU,MADLEN*+3
C
C*    ДЛИHA TAБЛИЦЫ ИДETИФИKATOPOB
C
  IDTL:,EQU,4096
 NLIDTL:,P/P,(IDTL)2000B
C
 SCRATCH*:,BSS,
 DISCRIPT:,ENTRY,
 14,VTM,STARTMDL
 ,ITA,14
 ,ATX,OUTMADLE
 2,VJM,GETPARAM
 ,XTA,INFSYS
 14,VTM,-7
 ,UZA,SYSLD
 :14,XTA,SNAME+7
 14,ATX,STNAM*+7
 14,VLM,*-1
 13,VJM,SPY
 ,XTA,INFSYS
 ,ASN,67+24
 ,AAX,RCON17
 ,ATX,GLOB*
 ,XTA,INFSYS
 ,AAX,CON2
 ,UZA,LOADMAC
 ,XTA,INFSYS
 ,AAX,CONR18
 ,ARX,INFSEC
 ,ATX,INFSEC
 12,VTM,1-NLIDTL
 LOADID:,*70,INFSEC
 ,XTA,INFSEC
 ,ARX,NEXTL
 ,ATX,INFSEC
 12,VLM,LOADID
 LOADMAC:,XTA,INFSYS
 ,AAX,CON1
 ,UZA,SYSLD
 ,*70,46B
 ,XTA,INFSYS
 ,AAX,CONR18
 ,ARX,INFDEC
 6,VTM,STROKA
 ,ATX,INFDEC
 ,*70,INFDEC
 7,VTM,-511
 8,VTM,TABDEC
 LOADM1:8,XTA,
 ,UZA,SYSLD-1
 11,VTM,
 12,VTM,LOADM20
 3,UTC,
 13,VJM,
 LOADM0:8,XTA,1
 ,AAX,CONR15
 ,UZA,LOADOPS
 ,XTA,W17
 ,AAX,CONR15
 ,ASN,64-24
 ,AOX,MACJMP
 14,XTS,1
 ,AAX,CONR15
 15,AEX,
 14,UTC,1
 ,CTX,
 15,XTA,
 8,XTA,1
 ,ASN,64+15
 1,AOX,-1
 ,WTC,W17
 ,CTX,
 ,XTA,W17
 ,ARX,RCON1
 ,ATX,W17
 8,XTA,1
 ,AAX,CONR15
 ,ASN,64+10
 ,AEX,RECORD
 ,UZA,LOADM3
 ,AEX,RECORD
 13,VJM,READZ
 LOADM3:8,XTA,1
 ,AAX,RMASK10
 ,ATI,12
 LOADM4:13,VJM,GETWORD
 13,VJM,PUTWORD
 ,AEX,RCON1
 ,U1A,LOADM4
 LOADMLP:8,UTM,2
 7,VLM,LOADM1
 ,UJ,SYSLD-1
 LOADM20:11,VZM,LOADM21
 8,XTA,
 11,ATX,
 ,XTA,
 11,ATX,1
 11,MTJ,14
 ,UJ,LOADM0
 LOADM21:14,XTA,1
 ,AEX,W17
 ,AAX,CONR15
 14,AEX,1
 14,UTC,1
 ,CTX,
 ,WTC,W17
 14,VTM,
 8,XTA,
 14,ATX,
 ,XTA,
 14,ATX,1
 ,XTA,W17
 ,ARX,RCON2
 ,ATX,W17
 ,UJ,LOADM0
 LOADOPS:8,XTA,1
 ,ASN,64+24
 ,AAX,CONR15
 15,ATX,
 ,ASN,64+1
 ,ATI,12
 ,ASN,64-1
 15,AEX,
 ,ATI,11
 1,UTC,
 12,XTA,
 11,VZM,LOADOP1
 ,ASN,64-24
 LOADOP1:,AAX,CONISK
 8,XTS,1
 ,AAX,CONR24
 15,AOX,
 ,AOX,MACJMP
 14,XTS,1
 ,AAX,CONR15
 15,AEX,
 14,UTC,1
 ,CTX,
 15,XTA,
 ,UJ,LOADMLP
 READZ:,ATX,RECORD
 ,ARX,INFSYS
 ,AAX,CONR18
 ,XTS,43B
 ,ASN,64-20
 ,AOX,CON001
 15,AOX,
 ,ATX,INFSEC
 ,*70,INFSEC
 13,UJ,
 GETWORD:,WTC,43B
 12,XTA,
 12,UTM,-1777B
 12,VZM,GETWORD1
 12,UTM,2000B
 13,UJ,
 GETWORD1:,ITS,13
 ,XTS,RECORD
 ,ARX,RCON1
 13,VJM,READZ
 15,XTA,
 ,STI,13
 13,UJ,
 PUTWORD:,WTC,W17
 ,ATX,
 ,XTS,W17
 ,ARX,RCON1
 ,STX,W17
 13,UJ,
C
C*******  ЗAПИCЬ TPAHCЛЯTOPA HA MБ
C
 :,*70,45B
 SYSLD:6,VTM,STROKA
 ,XTA,W17
 ,ATX,SAV17
 ,AAX,CONR15
 ,ATX,SIGNS
 ,ARX,CNR4010
 ,AAX,CONR15
 ,ATX,EXT
 ,XTA,W16
 ,ATX,SAV16
 ,AAX,CONR15
 ,ATX,EXTMAX
 ,ITA,15
 ,ATX,SAVI15
 ,XTA,NGZB
 ,ASN,NABEG+63
 ,XTS,SIGNS
 ,ASN,64+10
 ,ATI,14
 ,XTA,NGZB
 14,ASN,64
 15,AEX,
 ,ATX,MEMMASK
 ,ACX,INF*70
 ,ATX,SIGNS
 ,XTA,INFWDR
 ,AEX,CON001
 ,ATX,RA
 ,XTA,CON4
 ,ASN,NABEG+63
 ,AEX,MEMMASK
 ,XTS,CON4
 ,ASN,NADEC+63
 15,AEX,
 ,XTS,CON4
 ,ASN,NAINF+63
 ,ATX,RECORD
 15,AEX,
 ,ATX,MONSTART . OT MONITOR*
 ,XTA,CON74
 ,ASN,NASEC+63
 ,XTS,NGZB
 ,ASN,NAEND+63
 15,AOX,
 ,AAX,MEMMASK
 ,ATX,MDLSTART . OT MADLEN*
 ,*70,INFWDR
 ,XTA,RECORD
 11,VTM,
 ,AEX,MEMMASK
 ,ATX,MDLSTART
 ,XTA,CON001
 ,AEX,INFMEM
 ,ATX,INFMEM
 ,UJ,STARTMDL+2
 GETPARAM:,BSS,
 ,XTA,PASSWORD
 ,AEX,PASS
 ,UZA,GETPAR1
 ,XTA,
 ,ATX,INFSYS
 ,ATX,EDIT
 ,XTA,LISTF
 ,ATX,LIST
 ,XTA,MDPT
 ,ATX,MOD
 ,ATX,DD
 ,UTC,PAGELEN
 ,XTA,
 ,ATX,PAGE
 GETPAR1:,BSS,
 ,*50,67B
 ,ATX,SAVEDAT
 15,ATX,
  ,AUX,MXX/YY
 ,AEX,DATE1
 ,STX,DATE1
  ,ASN,64-14
 15,ATX,
  ,AUX,M/YY
 ,AEX,DATE1+1
 ,STX,DATE1+1
  ,ASN,64-8
 15,ATX,
  ,AUX,MXX/YY
 ,AEX,TIME1
 ,STX,TIME1
  ,ASN,64-14
  ,AUX,M/YY
 ,AEX,TIME1+1
 ,ATX,TIME1+1
 ,XTA,SAVEDAT
 ,ASN,64+16
 ,XTS,SAVEDAT
 ,AAX,CON00776
 15,AEX,
 ,ASN,64+2
 ,XTS,SAVEDAT
 ,ASN,64-14
 ,ATX,TME
 ,AAX,CON776
 15,AEX,
 ,AUX,GOSTBL
 ,AEX,ZISO0
 ,ATX,DTE
 ,XTA,TME
 ,ASN,64-10
 ,ASN,64+2
 ,AUX,GOSTBL
 ,AEX,ZISO0
 ,ATX,TME
 ,XTA,MOD
 ,ATX,MODTEXT
 ,AEX,MDPT
 ,ARX,MSK*177
 ,AAX,MSK*200
 ,ASN,64+7
 ,ARX,MSK*177
 ,AEX,MSK*177
 ,AAX,MOD
 ,ATX,MODEL
 ,XTA,DD
 ,ATX,DDT
 ,AEX,MDPT
 ,ARX,MSK*177
 ,AAX,MSK*200
 ,ASN,64+7
 ,ARX,MSK*177
 ,AEX,MSK*177
 ,AAX,DD
 ,ATX,DDE
 ,XTA,PAGE
 ,UTC,PAGELEN
 ,ATX,
 13,VJM,PRINT8**+4
 ,XTA,
 ,ATX,LISTECK
 ,XTA,EDIT
 ,ATX,EDITF
 ,AAX,LIS*D
 ,ATX,DEBUG
 ,XTA,LIST
 ,ATX,LISTF
 ,AAX,RCON1
 2,U1A,
 ,XTA,LISTF
 ,ASN,64+1
 ,AAX,RCON1
 ,ATX,LISMAD
 2,U1A,
 ,XTA,LISTF
 ,ASN,64-16
 ,U1A,T1INP
 ,ATX,LISMASK
 ,XTA,LIS*B
 ,ATX,LISTAND
 ,ATX,LISMAD
 :,ATX,LISLAST
 2,UJ,
 T1INP:,ASN,64+16
 ,ATX,LISMAD
 ,AAX,LISMASK0
 ,AEX,LISTAND
 ,ATX,LISTAND
 ,UJ,T1INP-1
 RECORD:,OCT,
 MACJMP  :,OCT,030
 INFSEC:,010,NSEC
 ,,
 INFDEC:,010,NDEC
 ,,NLIDTL
 INFWDR:,,NINF
 ,010,NAINF-1
 PASSWORD:,TEXT,8H MADLEN
 CON00776:,OCT,00776
 CNR4010:,LOG,4010
 GOSTBL:,GOST,6H
  MXX/YY:,GOST,6H03 0
  M/YY:,GOST,6H0  000
  MDPT:,ISO,6H******
 :,BSS,SCRATCH*+1000B-*
 MSTACK:,BSS,
 BUFPER:,ENTRY,
 2,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ПEPФOPAЦИЯ CTAHДAPTHOГO MACCИBA.
 *PUNCHER:,ENTRY,
 STAPUNCH:,ITA,13
 ,ATX,STARET
 1,VTM,ENTRY*
 1,XTA,
 ,ATX,STANAME
 1,XTA,1
 8,VTM,PUNADR
 13,VJM,LIBREAD5
 ,XTA,STANAME
 13,VJM,PSAINIT
 STAPUN1:1,XTA,
 ,UZA,STAPUN2
 13,VJM,PSASET
 1,XTA,1
 ,AAX,CONR15
 ,AOX,CON2
 13,VJM,PSASET
 1,UTM,1
 1,VLM,STAPUN1
 STAPUN2:,BSS,
 13,VJM,LIBREAD2
 ,ATX,STASTR
 ,ASN,64+12
 ,ATX,STASTR+1
 ,ASN,64+12
 ,ATX,STASTR+2
 ,ASN,64+12
 ,ATX,STASTR+3
 13,VJM,LIBREAD2
 ,ATX,STASTR+9
 ,ASN,64+15
 ,ATX,STASTR+8
 ,ASN,64+15
 ,ATX,STASTR+4
 13,VJM,LIBREAD2
 ,ATX,STASTR+5
 ,ASN,64+15
 ,ATX,STASTR+6
 ,ASN,64+15
 ,ATX,STASTR+7
 14,VTM,-3
 STAPUN3:14,XTA,STASTR+3
 ,AAX,CONR12
 14,ATX,STASTR+3
 14,VLM,STAPUN3
 14,VTM,1-6
 1,VTM,1-10
 STAPUN4:14,XTA,STASTR+9
 ,AAX,CONR15
 14,ATX,STASTR+9
 14,VLM,STAPUN4
 ,XTA,
 ,ATX,STALEN
 STAPUN5:1,XTA,STASTR+9
 13,VJM,PSASET
 1,XTA,STASTR+9
 ,ARX,STALEN
 ,ATX,STALEN
 1,VLM,STAPUN5
 ,NTR,3
 ,A-X,STASTR+6
 ,ATX,STALEN
 STAPUN6:,XTA,STALEN
 ,ARX,NG1B
 ,ATX,STALEN
 13,VTM,STAPUN7
 ,UZA,LIBREAD2
 13,VJM,LIBREAD2
 13,VTM,STAPUN6
 ,UJ,PSASET
 STAPUN7:13,VJM,PSAEND
 ,*70,45B
 ,WTC,STARET
 ,UJ,
C.......
 LIBREAD1:,XTA,50B
 ,AOX,CON001
 ,ATX,LIBR1
 ,ARX,LENTHCAT
 ,ATX,LIBR2
 ,*70,LIBR1
 1,VTM,
 12,VTM,TSYST
 11,VJM,LIBCAT
 11,VJM,LIBCAT
 :,AEX,STANAME
 ,UZA,LIBREAD3
 ,UJ,LIBCAT
 :12,VTM,TSYST
 LIBREAD3:,XTA,
 12,ATX,
 1,VTM,TSYST
 1,XTA,
 13,UZA,
 1,XTA,1
 LIBREAD5:,BSS,
 ,ASN,64-2
 ,AUX,RCON1740
 ,ATI,2
 1,XTA,1
 ,ASN,64-6
 ,AUX,CONLIB
 1,XTS,1
 ,AAX,CON4
 ,ASN,64+9
 15,AOX,
 ,XTS,43B
 ,ASN,64-20
 ,AOX,CON001
 15,AOX,
 ,ATX,LIBR1
 ,*70,LIBR1
 13,UJ,
 LIBCAT:,WTC,43B
 1,XTA,1
 15,ATX,
 ,AAX,MASK33
 ,UZA,LIBCAT1
 12,VTM,TSYST
 LIBCAT1:,WTC,43B
 1,XTA,
 ,UZA,LIBREAD3-1
 12,STX,
 12,ATX,1
 ,AEX,NGZB
 ,UZA,LIBREAD3-1
 12,XTA,
 12,UTM,2
 1,UTM,-1776B
 1,VZM,LIBCAT2
 1,UTM,2000B
 11,UJ,
 LIBCAT2:,XTS,LIBR1
 ,ARX,RCON1
 ,ATX,LIBR1
 ,AEX,LIBR2
 ,UZA,ERRORLIB+1
 ,*70,LIBR1
 15,XTA,
 11,UJ,
 LIBREAD2:,WTC,43B
 2,XTA,
 2,UTM,-1777B
 2,VZM,LIBREAD4
 2,UTM,2000B
 13,UJ,
 LIBREAD4:,XTS,LIBR1
 ,ARX,RCON1
 ,ATX,LIBR1
 ,*70,LIBR1
 15,XTA,
 13,UJ,
C.........
 STANAME:,BSS,1
 LIBR1  :,BSS,1
 LIBR2  :,BSS,1
 STALEN :,BSS,1
 STARET :,BSS,1
 STASTR :,BSS,10
 TSYST  :,BSS,50
C.........
 PSAINIT:,ATX,PSANAME
 ,ATX,PSARR1
 ,XTA,
 ,ATX,PSABCNT
 12,VTM,1-8
 10,VTM,
 ,ITA,13
 ,XTS,PSACON12
 ,ATX,PSACNTRL
 PSAINIT1:,XTA,PSARR1
 ,ASN,64-6
 ,ATX,PSARR1
 ,YTA,
 ,ASN,64-1
 ,ATI,14
 14,XTA,PUNCTABL
 13,VJM,PSASET
 14,XTA,PUNCTABL+1
 13,VJM,PSASET
 12,VLM,PSAINIT1
 15,XTA,
 ,ATI,13
 13,UJ,
 PSASET:10,ATX,PSABODY
 10,UTM,-15
 10,VZM,PSAFORM
 10,UTM,16
 13,UJ,
 PSAFORM:,XTA,PSAMASK
 ,ATX,PSARR3
 9,VTM,1-12
 :,XTA,
 ,ATX,PSARR1
 ,ATX,PSARR2
 10,VTM,-18
 11,VTM,
 PSAFORM1:10,XTA,PSACNTRL+18
 ,APX,PSARR3
 11,AEX,PSARR1
 ,ASN,64-4
 15,ATX,
 ,YTA,
 15,AEX,
 11,ATX,PSARR1
 10,UTM,10
 :10,V1M,*+1
 11,VTM,1
 :10,UTM,-9
 10,V1M,PSAFORM1
 ,XTA,PSARR3
 ,ASN,64-1
 ,ATX,PSARR3
 ,XTA,PSARR1
 ,ASN,64-12
 ,AUX,PSAUNP1
 15,ATX,
 ,ACX,
 ,XTS,PSARR2
 ,ASN,64-12
 ,AUX,PSAUNP2
 ,ATX,PSARR2
 15,ACX,
 ,AAX,PSACON1
 ,AEX,PSACON1
 ,ASN,64+11
 ,YTA,
 15,AOX,
 ,ATX,PSARR1
 12,VJM,PSABUFF
 9,VLM,PSAFORM+2
 ,XTA,PSACNTRL
 ,ARX,PSA10000
 ,ATX,PSACNTRL
 12,VTM,
 13,UJ,
 PSABUFF:,XTA,PSARR1
 ,WTC,PSABCNT
 8,ATX,
 ,XTA,PSARR2
 ,WTC,PSABCNT
 8,ATX,1
 ,XTA,PSABCNT
 ,ARX,PSACON2
 ,ATX,PSABCNT
 ,AEX,PSA1008
 12,U1A,
 PSABUFF1:,WTC,PSABCNT
 11,VTM,-1
 8,J+M,11
 ,XTA,
 11,ATX,1
 11,ATX,2
 ,ATX,PSABCNT
 ,*71,INFPUNCH
 12,UJ,
 PSAEND:,ATX,PSARR1
 ,ITA,13
 ,XTS,PSACNTRL
 ,AOX,PSACON1
 ,ATX,PSACNTRL
 ,XTA,PSARR1
 13,VJM,PSASET
 :,XTA,
 10,V1M,PSASET
 15,XTA,
 ,ATI,13
 PSABUF*:,XTA,PSABCNT
 13,UZA,
 13,MTJ,12
 ,UJ,PSABUFF1
 INFPUNCH:8,Z01,
  11,Z00,
 PSABCNT:,BSS,1
 PSACNTRL:,BSS,1
 PSABODY:,BSS,16
 PSANAME:,BSS,1
 PSARR1:,BSS,1
 PSARR2:,BSS,1
 PSARR3:,BSS,1
 PSACON1:,LOG,1
 PSACON2:,LOG,2
 PSACON12:,OCT,12
 PSAMASK:,OCT,0001000100010001
 PSAUNP1:,OCT,0017776776776776
 PSAUNP2:,OCT,0017757757757757
 PSA10000:,OCT,0000 0001
 PSA1008:,Z00,
 ,Z00,1008
 PUNCTABL:,BSS,
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT,0000 1774 1044 1044      .Б,1
         :  ,OCT,1044 1044 1044 0704      .Б,2
         :  ,OCT,0000 1774 1000 1000      .Ц,1
         :  ,OCT,1000 1000 1774 1000      .Ц,2
         :  ,OCT,1000 1740 1020 1010      .Д,1
         :  ,OCT,1004 1004 1004 1774      .Д,2
         :  ,OCT,0000 0360 0410 0410      .Ф,1
         :  ,OCT,1774 0410 0410 0360      .Ф.2
         :  ,OCT,0000 1774 0004 0004      .Г,1
         :  ,OCT,0004 0004 0004 0004      .Г,2
         :  ,OCT,0000 1774 0400 0200      .И,1
         :  ,OCT,0100 0040 0020 1774      .И,2
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT,0000 0000 0520 0340      .*,1
         :  ,OCT,0760 0340 0520 0000      .*,2
         :  ,OCT,0000 1774 0400 0204      .Й,1
         :  ,OCT,0104 0044 0020 1774      .Й,2
         :  ,OCT,0000 1740 0020 0010      .Л,1
         :  ,OCT,0004 0004 0004 1774      .Л,2
         :  ,OCT,0000 1070 0504 0304      .Я,1
         :  ,OCT,0104 0104 0104 1774      .Я,2
         :  ,OCT,0000 1014 0420 0240      .Ж,1
         :  ,OCT,1774 0240 0420 1014      .Ж,2
         :  ,OCT,0000 0000 0400 0200      ./,1
         :  ,OCT,0100 0040 0020 0000      ./,2
         :  ,OCT,0000 0770 1004 1004      .0,1
         :  ,OCT,1004 1004 1004 0770      .0,2
         :  ,OCT,0000 0000 1020 1010      .1,1
         :  ,OCT,1774 1000 1000 0000      .1,2
         :  ,OCT,0000 1030 1004 1404      .2,1
         :  ,OCT,1004 1204 1004 1170      .2,2
         :  ,OCT,0000 0404 1004 1044      .3,1
         :  ,OCT,1044 1024 1114 0604      .3,2
         :  ,OCT,0000 0100 0140 0134      .4,1
         :  ,OCT,0100 0100 0100 1774      .4,2
         :  ,OCT,0000 0474 1044 1044      .5,1
         :  ,OCT,1044 1044 1044 0704      .5,2
         :  ,OCT,0000 0760 1050 1044      .6,1
         :  ,OCT,1044 1044 1044 0710      .6,2
         :  ,OCT,0000 1004 0404 0304      .7,1
         :  ,OCT,0104 0144 0024 0014      .7,2
         :  ,OCT,0000 0730 1044 1044      .8,1
         :  ,OCT,1044 1044 1044 0730      .8,2
         :  ,OCT,0000 0470 1104 1104      .9,1
         :  ,OCT,1104 1104 1104 0770      .9,2
         :  ,OCT,0000 1774 1040 1040      .Ь,1
         :  ,OCT,1040 1040 1040 0700      .Ь,2
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT,0000 1774 0004 0004      .П,1
         :  ,OCT,0004 0004 0004 1774      .П,2
         :  ,OCT  ,
         :  ,OCT  ,                       .
         :  ,OCT  ,
         :  ,OCT  ,
         :  ,OCT,0000 1774 1040 1040      .Ы,1
         :  ,OCT,1040 0700 0000 1774      .Ы,2
         :  ,OCT,0000 0610 1004 1044      .З,1
         :  ,OCT,1044 1044 1044 0730      .З,2
         :  ,OCT  ,0000 1740 0220 0210    .A1
         :  ,OCT  ,0204 0204 0204 1774    .A2
         :  ,OCT  ,0000 1774 1044 1044    .B1
         :  ,OCT  ,1044 1044 1044 0730    .B2
         :  ,OCT  ,0000 0770 1004 1004    .C1
         :  ,OCT  ,1004 1004 1004 0610    .C2
         :  ,OCT  ,0000 1774 1004 1004    .D1
         :  ,OCT  ,1004 1010 0420 0340    .D2
         :  ,OCT  ,0000 1774 1104 1104    .E1
         :  ,OCT  ,1104 1104 1004 1000    .E2
         :  ,OCT  ,0000 1774 0104 0104    .F1
         :  ,OCT  ,0104 0104 0104 0004    .F2
         :  ,OCT  ,0000 0770 1004 1004    .G1
         :  ,OCT  ,1104 1104 1104 0710    .G2
         :  ,OCT  ,0000 1774 0100 0100    .H1
         :  ,OCT  ,0100 0100 0100 1774    .H2
         :  ,OCT  ,0000 0000 1000 1004    .I1
         :  ,OCT  ,1774 1004 1000 0000    .I2
         :  ,OCT  ,0000 0600 1000 1000    .J1
         :  ,OCT  ,1000 1004 1004 0774    .J2
         :  ,OCT  ,0000 1774 0040 0040    .K1
         :  ,OCT  ,0040 0120 0210 1404    .K2
         :  ,OCT  ,0000 1774 1000 1000    .L1
         :  ,OCT  ,1000 1000 1000 1000    .L2
         :  ,OCT  ,0000 1774 0010 0060    .M1
         :  ,OCT  ,0300 0060 0010 1774    .M2
         :  ,OCT  ,0000 1774 0020 0040    .N1
         :  ,OCT  ,0100 0200 0400 1774    .N2
         :  ,OCT  ,0000 0770 1004 1004    .O1
         :  ,OCT  ,1004 1004 1004 0770    .O2
         :  ,OCT  ,0000 1774 0104 0104    .P1
         :  ,OCT  ,0104 0104 0104 0070    .P2
         :  ,OCT  ,0000 0360 0410 1004    .Q1
         :  ,OCT  ,1204 1204 0410 1360    .Q2
         :  ,OCT  ,0000 1774 0104 0104    .R1
         :  ,OCT  ,0104 0304 0504 1070    .R2
         :  ,OCT  ,0000 0400 1030 1044    .S1
         :  ,OCT  ,1044 1044 1044 0710    .S2
         :  ,OCT  ,0000 0004 0004 0004    .T1
         :  ,OCT  ,1774 0004 0004 0004    .T2
         :  ,OCT  ,0000 0374 0400 1000    .U1
         :  ,OCT  ,1000 1000 0400 0374    .U2
         :  ,OCT  ,0000 0014 0060 0300    .V1
         :  ,OCT  ,1400 0300 0060 0014    .V2
         :  ,OCT  ,0000 0374 1400 0400    .W1
         :  ,OCT  ,0300 0400 1400 0374    .W2
         :  ,OCT  ,0000 1404 0210 0120    .X1
         :  ,OCT  ,0040 0120 0210 1404    .X2
         :  ,OCT  ,0000 0014 0020 0040    .Y1
         :  ,OCT  ,1700 0040 0020 0014    .Y2
         :  ,OCT  ,0000 1404 1204 1104    .Z1
         :  ,OCT  ,1044 1024 1014 1004    .Z2
         :  ,OCT,0000 1774 1000 1000      .Ш,1
         :  ,OCT,1770 1000 1000 1774      .Ш,2
         :  ,OCT,0000 1004 1044 1044      .Э,1
         :  ,OCT,1044 1044 1044 0770      .Э,2
         :  ,OCT,0000 1774 1000 1770      .Щ,1
         :  ,OCT,1770 1000 1774 1000      .Щ,2
         :  ,OCT,0000 0074 0100 0100      .Ч,1
         :  ,OCT,0100 0100 0100 1774      .Ч,2
         :  ,OCT,0000 1774 0100 0770      .Ю,1
         :  ,OCT,1004 1004 1004 0770      .Ю,2
 BUFF:,BSS,
 :,BSS,STAPUNCH+2000B-*
 PRINTBF:,BSS,
 ,LOG,102
 ,LOG,102
 ,BSS,13
 ,ISO,6H  ''''''''
 ,BSS,756
 ,LOG,102
 ,OCT,7777777777777400
 ,OCT,
 ,OCT,
C
C     BOT ЭTY KAPTY HAДO ЗAMEHИTЬ
C     ДЛЯ YCTAHOBKИ ДPYГOЙ ДЛИHЫ
C     CTPAHИЦЫ.
C
 PAGELEN:,LOG,102
 ,BSS,PRINTBF+2000B-*
 2,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ПEPBЫЙ ЛИCT AБCOЛЮTHOГO MOДYЛЯ.
 INFOR:,BSS,
 MADLEN:6,BASE,STROKA
 12,VTM,MONSTART
 :,XTA,SAV16
 ,ATX,W16
 ,XTA,SAV17
 ,ATX,W17
 ,XTA,
 ,ATX,SCRATCH*-8
 :13,VTM,
 11,VTM,LISTEST
 MADLEN1:12,XTA,
 ,AAX,CON4
 ,UZA,MADLEN2
 ,*70,INFMEM
 13,XTA,
 ,XTA,INFMEM
 ,ASN,64-10
 ,ATI,13
 MADLEN2:,BSS,
 :,XTA,INFMEM
 ,ARX,NEXTL
 ,ATX,INFMEM
 12,XTA,
 ,ASN,64-1
 12,ATX,
 ,U1A,MADLEN1
 11,UJ,
 SAV16   :,OCT,
 SAV17   :,OCT,
 MEMMASK :,OCT,
 MONSTART:,OCT,
 MDLSTART:,OCT,
 INFMEM:,010,NLASS
 ,010,
 NEXTL:,OCT,0000010000000001
 *PUNCH:,ISO,6H*PUNCH
 *NOP:,ISO,6H*NO PU
 ,BSS,INFOR+24-*
 RR1:,OCT,
 RR2:,OCT,
 RR3:,OCT,
 MASK24:,OCT,77777777
 C/5:,OCT,3714631463146314
 C//5:,OCT,1314631463146315
 C4:,Z00,
 ,Z00,TABDEC+1
 NSTR    :   ,  OCT  ,
 ,LOG,52
         :   ,  OCT  ,
 MACN:,ISO,12H
 MRAC:,OCT,
 INDTABL :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
         :   ,  OCT  ,
 ,OCT,
 BSSCNT  :   ,  OCT  ,
 DATCNT  :   ,  OCT  ,
 FIXCNT  :   ,  OCT  ,
 FLAGTABL:   ,  OCT  ,61
 ILAST:,OCT,
 CON61:,OCT,61
 KOSCNT  :   ,  OCT  ,
 RCON12:,LOG,12
 CON66:,OCT,66
 CON005:,OCT,005
 CON041:,OCT,041
 CON042:,OCT,042
 CON043:,OCT,043
 CON045:,OCT,045
 NK      :   ,  OCT  ,
 NKABS   :   ,  OCT  ,
 NKCARD  :   ,  OCT  ,
 NKKOS   :   ,  OCT  ,1
 POSIND  :   ,  OCT  ,
 RMIST   :   ,  OCT  ,
 MSK370*5:,OCT,7617437076174
 MSK0*5:,OCT,140300601403
 MSK7*5:,OCT,01603407016034
 CON063:,OCT,063
 SETCNT  :   ,  OCT  ,
 CON066:,OCT,066
 CON076:,OCT,076
 YSLNYL  :   ,  OCT  ,
 *AB*TAB:,ISO,6H*FULL
 ,ISO,6H*NO LI
 ,ISO,6H*STAND
 ,ISO,6H*
 ,ISO,6H*NO MO
 ,ISO,6H*MOZY
 CON306:,OCT,306
 AB:,Z00,
 ,Z00,SECTOR+IDTL
 AR      :   ,  OCT  ,7776
 AH      :   ,  OCT  ,
 CON1    :   ,  OCT  ,1
 CON2    :   ,  OCT  ,2
 CON3    :   ,  OCT  ,3
 CON4    :   ,  OCT  ,4
 CON6    :   ,  OCT  ,6
 CON7    :   ,  OCT  ,7
 CON01   :   ,  OCT  ,01
 CON02   :   ,  OCT  ,02
 CON04   :   ,  OCT  ,04
 MASK33:,OCT,77777777 777
 CON72   :   ,  OCT  ,72
 CON001  :   ,  OCT  ,001
 CON002  :   ,  OCT  ,002
 CN0001  :   ,  OCT  ,000000001
 CN0002  :   ,  OCT  ,000000002
 CN0004  :   ,  OCT  ,000000004
 MASK37:,OCT,77777777 77774
 CN00001 :   ,  OCT  ,0000000001
 CN00002 :   ,  OCT  ,0000000002
 CN00003 :   ,  OCT  ,0000000003
 CN0005:,LOG,50000000
 CN00005 :   ,  OCT  ,0000000005
 CN00006 :   ,  OCT  ,0000000006
 CN00007 :   ,  OCT  ,0000000007
 MASK41:,OCT,77777777 777776
 CN000001:   ,  OCT  ,00000000001
 CN000002:   ,  OCT  ,00000000002
 CN000003:   ,  OCT  ,00000000003
 TOP:,LOG,41000000
 AA6SIMB :   ,  OCT  ,77
 AA6SIMBR:   ,  OCT  ,0000000000000077
 BSIMB   :   ,  OCT  ,0000000000000042
 RCON1   :   ,  OCT  ,0000000000000001
 CONR15  :   ,  OCT  ,0000000000077777
 MB40:,LOG,77740
 CNR4000 :   ,  OCT  ,0000000000004
 CNR7S12:,LOG,70000
 CONSS   :   ,  OCT  ,77770000777
 FLAG3B  :   ,  TEXT ,1H*
 RCON320:,LOG,320
 RCON20  :   ,  OCT  ,000000000000002
 TOP1:,OCT,10000000 41
 RCON70  :   ,  OCT  ,000000000000007
 CON004  :   ,  OCT  ,004
 RCN77766:,LOG,77766
 ERCNT   :   ,  OCT  ,
 CONR24  :   ,  OCT  ,0000000077777777
 CONR12  :   ,  OCT  ,0000000000007777
 CREAL10 :   ,  REAL ,10.0
 ZMPAR:,OCT,
 T*ЬЬЬЬ:,TEXT,8HЬЬЬЬ
 CONKP1  :   ,  OCT  ,00000000777
 RCONY   :   ,  OCT  ,0000000000074
 NKBSS   :   ,  OCT  ,1
 NKDATA  :   ,  OCT  ,
 MARDIS:,OCT,
 KBSSAD  :   ,  OCT  ,
 CON33:,OCT,33
 CON776  :   ,  OCT  ,776
 COMMA:,OCT,
 CONDLI1 :   ,  OCT  ,00000001
 CON43:,OCT,43
 RBLANK4 :   ,  OCT  ,0000004010020040
 CINT0   :   ,  INT  ,0                   .
 C701S6:,LOG,70100
 CON/6:,OCT,4002525252525253
 DEBUG:,OCT,
 DEBUGF:,OCT,
 BASKOP:,LOG,2400000
 OUTMADLE:   ,  OCT  ,
 MESREAD*:   ,  OCT  ,
 MWCNT:,OCT,
 VERETTI :   ,  OCT  ,
 BASEMOD:,OCT,
 BASE*:,OCT,
 BASIR:,OCT,
 BASPR:,OCT,
 UTCPR:,OCT,
 UTCREG:,OCT,
 GLOKOP:,OCT,
 YSLUTC:,OCT,
 BASYPR:,OCT,
 BASLONG:,OCT,
 UTCCONST:,OCT,0000 0000 0220 0000
 XBASE:,OCT,
 GLOB*:,OCT,
 LISMASK0:,LOG,15607277400
 BLBSS:,OCT,
 BLUTC:,OCT,
 VERET:,OCT,
 CON104:,OCT,104
 BASYSL:,OCT,
 NO*DEC:,OCT,047
  SOS:,OCT,
 NN:,OCT,
 IDFLAG:,OCT,
 IFFLAG:,OCT,
  XTEXTF:,OCT,
  LISTF:,LOG,2
  EDITF:,OCT,
  MODEL:,OCT,
 DDE:,OCT,
 VVE:,ISO,6HMDL3.5
  DTE:,OCT,
  TME:,OCT,
 CON774:,OCT,774
 DDT:,ISO,6HDD79''''
 INF*70:,010,NLASS
 ,010,
 RESETFL:,OCT,
 STEXTPNT:,OCT,
 STEXTCNT:,OCT,
 TITLE1:,OCT,
 ,ISO,12H
 ,ISO,12HMADLEN-3.5
 DATE1:,ISO,12H 00/00/00  ''
  TIME1:,ISO,12H 00.00.00
 ,ISO,12H CTP ''   1''''
 TITLE2:,BSS,11
 CONR17:,LOG,377777
  IFNAM  : ,OCT,
  HASH   : ,OCT,405 3016324127775
  PACKH  : ,OCT,405
  CUTHH  : ,LOG, 0077 7777 7777
 LONGSTR:,OCT,1000000000000110
  NG1B   : ,OCT, 7777 7777 7777 7776
  NGZB   : ,OCT, 7777 7777 7777 7777
 RCON17:,LOG,17
 CON534:,OCT,534
  IFADRF : ,OCT,
 EXPO**:,OCT,
 I1:,OCT,0001 0000 0000 0000
 ,OCT,1011 1236 0035 0017
 ,OCT,2021 2223 2425 2627
 ,OCT,3031 0033 0000 0000
 ,OCT,0041 4243 4445 4647
 ,OCT,5051 5253 5455 5657
 ,OCT,6061 6263 6465 6667
 ,OCT,7071 7200 0000 0000
 ,OCT,7741 0203 0445 0506
 ,OCT,7007 1353 1455 5057
 ,OCT,3415 6043 6471 1666
 ,OCT,3237 4073 7475 7600
 C13S15:,LOG,1300000
 *PROBEL:,ISO,H
 C52104S9:,LOG,52104000
 REN*FL:,OCT,
 C403S15:,LOG,40300000
 SAVEDAT:,OCT,
 FLGEX*:,OCT,
 ,BSS,39
 NCON01:,OCT,7677777777777777
 ,BSS,50
 ZR1:,OCT,
 ZR2:,OCT,
 ZR3:,OCT,
 C201S15:,LOG,20100000
 CN00014:,LOG,14000000
 CN00015:,LOG,15000000
 CN00016:,LOG,16000000
 CN00017:,LOG,17000000
 NCON6:,OCT,1777777777777777
 NCON7:,OCT,0777777777777777
 CN000044:,LOG,4400000
 CN000054:,LOG,5400000
 CN000064:,LOG,6400000
 CN000074:,LOG,7400000
 CN000102:,LOG,10200000
 MASK*06:,OCT,0606060606060606
 MASK*07:,OCT,0707070707070707
 MASK*17:,OCT,1717171717171717
 MASK*20:,OCT,2020202020202020
 MASK*40:,OCT,4040404040404040
 MASK*50:,OCT,5050505050505050
 MASK*60:,OCT,6060606060606060
 MASK*70:,OCT,7070707070707070
 EJTEXT:,TEXT,5HEJECT
 NCON04:,OCT,7377777777777777
 NCN0006:,OCT,7777777717777777
 NCON76:,OCT,0177 7777 7777 7777
 NCON77:,OCT,0077 7777 7777 7777
 M47S15:,OCT,7777 7777 7767 7777
 M33S47:,OCT,3777 7777 7774 0000
 RMASK11:,LOG,3777
 RMASK14:,LOG,37777
 RCON5:,LOG,5
 RCON7:,LOG,7
 RCON15:,LOG,15
 RCON150:,LOG,150
 OCT*PLG:,OCT,
 CONSS2:,OCT,7777 0000 7740 0000
 CONSS3:,OCT,7777 4000 7770 0000
 CONRV:,OCT,7777 4000 3777 7777
 CONW:,OCT,3777 7777 7775 0000
 CONBAS:,OCT,7000 0000 0000 7777
 CONNG:,LOG,40304000
 CONRL:,OCT,3777 7777 7774 4000
 CONCL:,OCT,3777 7777 3767 7777
 CON*T:,OCT,77377777774
 CONG:,OCT,3777 7777 7777 7401
 MODTEXT:,OCT,
 ABSCNT:,OCT,
 MACON:,OCT,0010 0000 0010 0000
 CNR10000:,LOG,10000
 AB*PLG:,OCT,
 RDLITS:,ISO,5H -RD-
 DEBMASK:,OCT,7777777727
 CONRE81:,OCT,1000 0000 1000 0000
 CONRE84:,OCT,0000000040004000
 CONSUB:,OCT,0100 0000 0300 0000
 CONSB:,OCT,0100 0000 0700 0000
 CONISK:,OCT,00077777
 LIS*BV:,OCT,0000 0100 0000 2000
 LIS*LR:,OCT,0000 0000 0404 0000
 NLIS*BRV:,OCT,7777 7677 7773 5777
  SSW:,TEXT,6HM
 ,TEXT,6HD
  ,TEXT,6HV
  ,TEXT,6HDT
  ,TEXT,6HTM
 RCON200:,LOG,200
 RCON377:,LOG,377
 R3B:,LOG,52
 SKIPFLAG:,OCT,                           .400
 LISTAND:,LOG,11005010000
 LISMASK:,LOG,15607277400
 RCON100:,LOG,100
 LISMAD:,OCT,
 LISLAST:,LOG,11005010000
 LIS*G:,OCT,0000 0002 0000 0000
 LIS*B:,OCT,0000 0100 0000 0000
 LIS*C:,OCT,0000 0040 0000 0000
 LIS*E:,OCT,0000 0010 0000 0000
 LIS*F:,OCT,0000 0004 0000 0000
 LIS*L:,OCT,0000 0000 0400 0000
 LIS*R:,OCT,0000 0000 0004 0000
 LIS*S:,OCT,0000 0000 0002 0000
 LIS*V:,OCT,0000 0000 0000 2000
 RDFLAG:,OCT,
 RCON6:,LOG,6
 RCON60:,LOG,60
 *EQPLG:,OCT,
 *KOPPLG:,OCT,
 CONR16:,LOG,177777
 NKMAX:,LOG,56000
 NCON1:,OCT,6777777777777777
 RCON50:,LOG,50
 CNR104S9:,LOG,104000
 CNR304S9:,LOG,304000
 CODPLG:,OCT,
 CON*31:,LOG,3100000
 GLOBCON:,OCT,2131272047
 NRC14:,LOG,777777777777763
 MODMASK:,OCT,777777777777776
 MASK44:,OCT,777777777777776
  PSKPLG:,OCT,
 IFPLG:,OCT,
 I:,OCT,7777761246377535
 ,OCT,0517771077714777
 ,OCT,4311451623615201
 ,OCT,4110260626210420
 ,OCT,0705712406651125
 ,OCT,2405141726656377
 ,OCT,2565312127611023
 ,OCT,0620501502607014
 ,OCT,0000040200602005
 ,OCT,0140341002217426
 ,OCT,0721243626177440
 ,OCT,1043047711240101
 ,OCT,1324110312442054
 ,OCT,2122706021443510
 ,OCT,1444451222632463
 ,OCT,2301377706046532
 ,OCT,1722004115422045
 ,OCT,1502146512024452
 ,OCT,1262605513427476
 ,OCT,1403046214623042
 ,OCT,1663504716036071
 ,OCT,1577777777777777
 I2:,OCT,0670600000013400
 ,OCT,0010560145506616
 ,OCT,4630363042111202
 ,OCT,4150762547505203
 ,OCT,4471622100016620
 ,OCT,0000000031666161
 ,OCT,1326745115075063
 ,OCT,1126046636034574
 ,OCT,3442106512677043
 ,OCT,1402315032427000
 ,OCT,0000000000102547
 ,OCT,2305051533644524
 ,OCT,3205150530253170
 ,OCT,3647612220471161
 ,OCT,3746512033044112
 ,OCT,2351221146512400
 NOLMASK:,OCT,7777767772737777
 CSSILK:,OCT,
 CON44:,OCT,44
 TLREG:,LOG,202
 ,LOG,065
 ,LOG,066
 TLREGC:,LOG,004
 TLREGR:,LOG,037
 TLREGL:,LOG,000
 TL0:,LOG,140
 TLZ:,LOG,056
 TLGRYS:,LOG,0707
 ,LOG,0614
 ,LOG,1321
 ,LOG,1717
 ,LOG,3132
 ,LOG,0101
 ,LOG,3030
 ,LOG,3434
 ,LOG,2522
 ,LOG,3636
 ,LOG,3210
 ,LOG,0312
 RCON40:,LOG,40
 RCON340:,LOG,340
 RCON10:,LOG,10
 TESTST:,TEXT,8HS2TEXT**
 CN000007:,LOG,700000
 MASK30:,OCT,7777777777
 NCON2:,OCT,5777777777777777
 NCON3:,OCT,4777777777777777
 NCON4:,OCT,3777777777777777
 LIS*D:,OCT,0000002000000000
 *COMPLG:,OCT,
 FLGADR:,OCT,
 ENDM*S:,ISO,12H :,ENDM,
 CN0006:,LOG,60000000
 ZISO0:,ISO,6H000000
 ZGOST7:,GOST,6H777777
 RCON2:,LOG,2
 CON74:,OCT,74
 EXT:,OCT,
 EXTMAX:,OCT,
 GCNT:,OCT,
 LCNT:,OCT,
 MSK*200:,ISO,6H'200''200''200''200''200''200'
 MSK*177:,ISO,6H'177''177''177''177''177''177'
 MSK*012:,ISO,6H'012''012''012''012''012''012'
 EXTFLG:,OCT,
 MACS5*:,ISO,5H MAC
 MACE5*:,ISO,5H MAC=
 *ЬЬЬЬ:,ISO,6HЬЬЬЬ''''
 MAINPRIN:,LOG,4000000
 CON777:,OCT,777
 LIS*MS:,LOG,2020000
 ZIRP:,OCT,
 IRPF1:,OCT,
 IRPF2:,OCT,
 IRPF3:,OCT,
 IRPF4:,BSS,12
 LOCTEXT:,TEXT,5HLOCAL
 ,TEXT,2HNE
 ,TEXT,2HGE
 ,TEXT,2HLT
 ,TEXT,2HGT
 ,TEXT,2HLE
 RENKL:,TEXT,2HEQ
 ,TEXT,2HEX
 ,TEXT,1HW
 ,TEXT,2HLC
 ,TEXT,2HPC
 ,TEXT,2HSC
 ,TEXT,2HLP
 ,TEXT,2HPP
 ,TEXT,2HSP
 ,TEXT,2HLU
 ,TEXT,2HPU
 ,TEXT,2HSU
 ,TEXT,2HLS
 ,TEXT,2HPS
 ,TEXT,2HSS
 ,TEXT,1HB
 ,TEXT,1HI
 CON0004 :,OCT,0004
 CON0002 :,OCT,0002
 CON0001 :,OCT,0001
 CON00004:,OCT,00004
 CON00002:,OCT,00002
 CON00001:,OCT,00001
 BADPARAM:,TEXT,8HNAME
 :,TEXT,8HLOCAL
 :,TEXT,8HIRP
 :,TEXT,8HEND
 :,TEXT,8HENDM
 :,TEXT,8HENDD
 MASK38:,OCT,7777 7777 7777 6000
 CON5:,OCT,5
 STEXTCOM:,BSS,6
 STNAM*:,BSS,8
 RCON3:,LOG,3
 FLFAT:,OCT,
 RSTNR:,OCT,
 RISO10:,ISO,6H    10
 D4B:,LOG,4021000000
 STAR3B:,OCT,00000052
 CONPRIL:,LOG,3227400
 RMASK26:,LOG,377777777
 C/10:,OCT,4001463146314632
 CTM:,BSS,5
 ,ISO,12H
 ,CONT,6H''''  CP
 ,OCT,1
 ,ISO,12H.'',''''''''''  ST
 ,OCT,1
 ,ISO,12H.'',''''''''''  RT
 ,OCT,1
 ,OCT,134
 MDNS*1:,OCT,
 MDNS*2:,OCT,
 SYSABS:,OCT,000000004
 SYSREL:,OCT,
 SYSMASK:,OCT,7770000077700000
 CN000004:,LOG,400000
 NCARD:,OCT,
 CON76:,OCT,76
  SAVI15:,OCT,
 AE:,OCT,
 CONR18:,LOG,777777
 CONDG:,OCT,043000001
 RMASK10:,LOG,1777
 RCON1740:,LOG,1740
 TOLIB1:,OCT,
 CONLIB:,OCT,0010 0000 0077 0037
 WLIB:,Z00,NLASS
 ,Z00,
 RE*RAB:,EQU,STROKA+247
 RE*RAB1:,EQU,STROKA+248
 LIS*N:,EQU,CN00001
 INITLIB:,ENTRY,
C
C
 BUFPER:,EQU,MSTACK
 FREELIB:,EQU,54B
 DPARAM:,EQU,STARTMDL+1
 LENTHCAT:,EQU,RCON3
 2,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ЗAПИCЬ BO BPEMEHHYЮ БИБЛИOTEKY.
C
C+++++++++++++++++++++++++++++++++++++++
C+
C+   ПPOЦEДYPЫ ЗAПИCИ BIANARY BO BPEMEH
C+   HYЮ БИБЛИOTEKY.
C+
C+
 INITLIB:,XTA,FREELIB
  ,ARX,RCON3
  ,ATX,TOLIB1
  ,AAX,RCON1740
 13,UZA,
  ,XTA,TOLIB1
  ,AOX,CN0001
 13,MTJ,12
 ,UJ,EXCHLIB
C+   ЗAПИCЬ OДHOГO CЛOBA B LIBRARY
 WTOLIB:,ENTRY,
 WTOLIB: ,XTS,TOLIB1
  ,AAX,RMASK10
 ,STI,14
 ,UTC,BUFPER
 14,ATX,
 14,UTM,-1777B
 14,V1M,WTOLIB1
  ,XTA,TOLIB1
 12,VJM,EXCHLIB
 WTOLIB1: ,XTA,TOLIB1
  ,ARX,RCON1
  ,ATX,TOLIB1
 13,UJ,
C+   OБMEHHAЯ OПEPAЦИЯ C LIBRARY
 EXCHLIB:15,ATX,
 ,ASN,64-26
  ,AUX,CONLIB
  ,AEX,WLIB
 15,ATX,2
  ,AAX,CN000001
 ,U1A,ERRLIB
 ,XTA,FLGEX*
 :,U1A,*+2
 15,*70,2
 15,XTA,
 12,UJ,
 :,XTA,UJ13
 ,CTX,WTOLIB
 15,UTM,-1
 ,CTX,STOLIB
 UJ13:13,UJ,
 STOLIB:,ENTRY,
 ,XTA,FLGEX*
 13,U1A,
C+   ЗAПИCЬ CTPYKTYPЫ
 STOLIB: ,XTA,TOLIB1
 ,AEX,FREELIB
 ,ASN,64+10
 ,UZA,STOLIB1
  ,XTA,TOLIB1
 12,VJM,EXCHLIB
 ,XTA,FREELIB
  ,AOX,CN0001
 12,VJM,EXCHLIB
 STOLIB1:,XTA,FREELIB
  ,AAX,RMASK10
 ,ATI,14
 14,UTM,BUFPER
 ,XTA,STROKA+3
 ,ASN,64-12
 ,AOX,STROKA+2
 ,ASN,64-12
 ,AOX,STROKA+1
 ,ASN,64-12
 ,AOX,STROKA
 14,ATX,
 ,XTA,STROKA+4
 ,ASN,64-15
 ,AOX,STROKA+8
 ,ASN,64-15
 ,AOX,STROKA+9
 14,ATX,1
 ,XTA,STROKA+7
 ,ASN,64-15
 ,AOX,STROKA+6
  ,ASN,64-15
 ,AOX,STROKA+5
 14,ATX,2
 ,XTA,FREELIB
  ,AAX,RCON1740
 ,ASN,64-16
 ,AOX,FREELIB
  ,AAX,MASK38
 ,ASN,64-20
 ,XTS,STROKA+7
 ,ARX,STROKA+6
 ,ARX,STROKA+5
  ,AAX,CONR15
 ,ASN,64-15
 15,AOX,
 ,ATX,ENTRY*+1
 ,XTA,FREELIB
 12,VJM,EXCHLIB
  ,XTA,TOLIB1
  ,ARX,TLREGR
 ,ASN,64+5
 ,ASN,64-5
 ,ATX,FREELIB
 10,VTM,ENTRY*
 2,VTM,
 3,VTM,
  ,ATX,RR1
 ,XTA,
  ,ATX,RR2
 12,VJM,ETOLIB
 :10,XTA,
 ,UZA,CTOLIB
  ,ATX,RR1
 10,XTA,1
  ,AEX,CON2
  ,ATX,RR2
 10,UTM,2
 ,UJ,ETOLIB
 CTOLIB:10,VTM,ENTRY*
 12,VTM,DPARAM
 :10,XTA,
 12,ATX,
 ,UZA,*+2
 10,UTM,2
 12,VLM,CTOLIB+1
 :,XTA,50B
 ,AOX,CON001
 ,ATX,RE*RAB
  ,ATX,RE*RAB
 ,ASN,64-24
 ,ASN,64+24
  ,AEX,WLIB
  ,ATX,RE*RAB1
 ,ARX,LENTHCAT
 ,ATX,MACON
 ,*70,46B
 11,VJM,OLDLIB
 CTOLIB2:10,VTM,DPARAM
 11,VJM,OLDLIB
 :10,XTA,
 ,UZA,ENDLIB
  ,AEX,RR1
 ,UZA,CTOLIB3
 10,UTM,1
 ,UJ,CTOLIB2+1
 CTOLIB3:10,XTA,1
 10,ATX,
 ,UZA,CTOLIB2
 10,VLM,CTOLIB3
 ENDLIB:10,UTM,-DPARAM
  ,XTA,RR2
  ,AAX,CONR15
  ,XTS,RR3
  ,AAX,MASK33
 15,AEX,
  ,ATX,RR2
 ,XTA,
  ,ATX,RR3
 10,VZM,ENDLIB1
 12,VTM,CTOLIB2
 ,UJ,ETOLIB
 ENDLIB1:12,VTM,OLDLIB
 11,VTM,ENDLIB1
 ETOLIB: ,XTA,RR1
 ,UTC,BUFPER
 2,ATX,
  ,XTA,RR2
 ,UTC,BUFPER
 2,ATX,1
 2,UTM,-1776B
 2,VZM,ETOLIB1
 2,UTM,2000B
 12,UJ,
 ETOLIB1: ,XTA,RE*RAB1
 ,AEX,MACON
 ,AAX,CONR12
 ,UZA,ERRLIB1
  ,*70,RE*RAB1
  ,XTA,RE*RAB1
  ,ARX,RCON1
  ,ATX,RE*RAB1
 12,UJ,
 OLDLIB:3,V1M,OLDLIB1
 ,XTA,RE*RAB
 ,AEX,MACON
 ,AAX,CONR12
 ,UZA,ERRLIB1
 ,*70,RE*RAB
 ,XTA,RE*RAB
 ,ARX,RCON1
 ,ATX,RE*RAB
 OLDLIB1:,WTC,43B
 3,XTA,
  ,ATX,RR1
 ,UZA,OLDLIB2
 ,WTC,43B
 3,XTA,1
  ,ATX,RR2
  ,AEX,NGZB
 ,UZA,OLDLIB2
  ,XTA,RR2
 ,ASN,64+15
 ,UZA,OLDLIB*
  ,XTA,RR2
 ,ATX,RR3
 OLDLIB*:3,UTM,-1776B
 11,UTC,
 3,VZM,
 3,UTM,2000B
 11,UJ,
 OLDLIB2:,XTA,
 ,UTC,BUFPER
 2,ATX,
  ,XTA,NGZB
 ,UTC,BUFPER
 2,ATX,1
 12,VJM,ETOLIB1
 ,XTA,EDITF
 ,AAX,CN000002
 ,UZA,LIBRET
 ,*70,INF*70
 ,UJ,STAPUNCH
 LIBRET:,*70,45B
 13,UJ,
 ERRLIB:,XTA,UJ13
 ,CTX,WTOLIB
 ,XTA,UJ13
 ,CTX,STOLIB
 ,UTC,-1
 ERRLIB1:14,VTM,ERRORLIB+1
 ,ITA,14
 ,ATX,OUTMADLE
 13,UJ,
 ERRORLIB:,UTC,ELIB1-ELIB2
 :14,VTM,ELIB2
 ,ITS,14
 14,UTM,6
 ,ITS,14
 ,XTS,RCON1
 ,CALL,PRINT8
 ,SJ,
 2,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ЗAГPYЗKA AБCOЛЮTHOГO MOДYЛЯ.
 STARTMDL:11,VTM,TESPROP
 ,*70,RA
 /MACPAR/:,ENTRY,
 ,WTC,SAVI15
 15,VTM,
 ,XTA,MONCARD*
 ,ASN,64-8
 ,YTA,
 12,VTM,MDLSTART
 ,ATI,14
 14,UTM,-40B
 14,VZM,MADLEN1-1
 ,XTA,MONCARD*
 14,UTM,40B-103B
 14,V1M,NOCOMT
 ,AEX,CON306
 ,ATX,MONCARD*
 PRIMC:14,VTM,MONCARD*
 ,ITS,14
 14,UTM,13
 ,ITS,14
 ,ITS,
 13,VTM,*+2
 11,VZM,PRINT8
 ,UJ,PRINT8 .БYДET ИHAЧE
 :13,VTM,STARTMDL+2
 ,UJ,MONREAD*
 NOCOMT:,AEX,*PUNCH
 14,UTM,103B-52B
 14,V1M,MADLEN1-1
 ,U1A,*+3
 ,XTA,CN000002
 ,ATX,PUNCHFL*
 ,AOX,EDITF
 ,UJ,*+5
 :,XTA,MONCARD*
 ,AEX,*NOP
 ,U1A,*+4
 ,ATX,PUNCHFL*
 ,XTA,CN000002
 ,AAX,EDITF
 ,AEX,EDITF
 :,ATX,EDITF
 ,UJ,PRIMC
 :,XTA,MONCARD*
 ,AEX,*AB*TAB
 ,U1A,*+2
 ,XTS,RCON1
 FUNO:,STX,FULISFL*
 ,UJ,FUNO1
 :,XTA,MONCARD*
 ,AEX,*AB*TAB+1
 ,U1A,*+2
 ,XTA,RCON1
 ,ITS,
 ,UJ,FUNO
 :,XTA,MONCARD*
 ,AEX,*AB*TAB+2
 ,U1A,*+2
 ,ATX,FULISFL*
 FUNO1:,ATX,NOLISFL*
 ,UJ,PRIMC
 :,XTA,MONCARD*
 ,AEX,*AB*TAB+3
 ,UZA,PRIMC
 11,UTC,
 11,V1M,
 11,VTM,MONITOR*+1
 13,VTM,
 ,UJ,MADLEN1
 TESPROP:,XTA,LISTAND
 ,AAX,CN000002
 ,UZA,MONITOR*+1
 ,UJ,MONITOR*+1 .БYДET ИHAЧE
 LISTEST:,XTA,LISMAD
 ,U1A,RUN
 ,XTA,NOLMASK
 ,AAX,LISTAND
 ,XTS,NOLISFL*
 ,ARX,NGZB
 ,AEX,NOLISFL*
 ,AAX,LIS*L
 15,AEX,
 ,XTS,FULISFL*
 ,ARX,NGZB
 ,AEX,FULISFL*
 ,AAX,LIS*LR
 ,AEX,LIS*LR
 15,AOX,
 ,XTS,*REGIM//
 ,AAX,CON4
 ,ARX,NGZB
 ,AAX,LIS*B
 15,AEX,
 ,XTS,*REGIM//
 ,AAX,CON2
 ,ARX,NGZB
 ,AAX,LIS*N
 15,AEX,
 ,XTS,*REGIM//
 ,AAX,CON01
 ,AEX,CON01
 ,ARX,NGZB
 ,AAX,LIS*G
 15,AEX,
 ,ATX,LISLAST
 ,ATX,LISTAND
 ,ATX,LISMASK
 ,UJ,RUN
 ELIB1:,ISO,36HПEPEПOЛHEHA BPEMEHHAЯ БИБЛИOTEKA''''''''
 ,OCT,
 ELIB2:,ISO,42HПEPEПOЛHEH KATAЛOГ BPEMEHHOЙ БИБЛИOTEKИ''''''
 QRESET:,ENTRY,
C
C*  QRESET - RESTORE KOPTAB AFTER PASS1
C
 QRESET1:,BSS,
 12,XTA,
 13,UZA,
 ,AAX,CONR15
 ,ATX,RR2
 12,XTA,
 ,ASN,64+24
 ,ATI,11
 11,MTJ,14
 ,XTA,NCON4
 ,ATX,NCON4
 QRESET2:14,XTA,
 ,UZA,QRESET3
 ,AAX,NCON4
 14,ATX,
 14,WTC,1
 14,VTM,
 14,V1M,QRESET2
 QRESET3:11,UTM,2
 ,ITA,11
 ,AEX,RR2
 13,UZA,
 11,MTJ,14
 ,UJ,QRESET2
 2,TITLE,ACCEMБЛEP MADLEN. ИHИЦИATOP. ГEHEPAЦИЯ CИCTEMHOГO TEKCTA.
C
C+  ГEHEPAЦИЯ CИCTEMHOГO TEKCTA
C
C****************************************
C*  CTPYKTYPA CИCTEMHOГO TEKCTA
C****************************************
C*  CИCTEMHЫЙ TEKCT COCTOИT TOЛЬKO ИЗ
C*  KOHCTAHT. ПEPBOE CЛOBO CИCTEMHOГO
C*  TEKCTA ИMEET BИД ,TEXT,8HS2TEXT**
C*  ДAЛEE ИДET KOMEHTAPИЙ ПPИЧEM ПEPBЫE
C*  48 PAЗPЯДOB COДEPЖAT ДATY И BPEMЯ
C*  MOMEHTA ГEHEPAЦИИ CИCTEMHOГO TEKCTA,
C*  ПPИЗHAKOM KOHЦA KOMEHTAPИЯ CЛYЖИT
C*  0B B KPAЙHEM ЛEBOM БAЙTE CЛOBA .
C*  MAKCИMAЛЬHAЯ ДЛИHA KOMEHTAPИЯ 10 CЛOB
C*  ПOCЛE KOMEHTAPHOГO ПOЛЯ ИДYT 'ГPYППЫ
C*  CИCTEMHOГO TEKCTA' , KAЖДAЯ ГPYППA
C*  HAЧИHAETCЯ C ЗAГOЛOBKA.
C*  ЗAГOЛOBOK ИMEET CЛEДYЮЩИЙ ФOPMAT:
C*  48-24 PAЗPЯДЫ ПOЛE, KOTOPOE MOЖET
C*  БЫTЬ ИCПOЛЬЗOBAHO 'ГPYППOЙ'
C*  24-16 PAЗPЯДЫ - HOMEP ГPYППЫ
C*  15-1  PAЗPЯДЫ - ДЛИHA ГYППЫ БEЗ
C*  ЗAГOЛOBKA.
C*         Г P Y П П Ы
C*  GLOBAL (0) ПPИЗHAK ГЛOБAЛЬHOГO БAЗИPOBAHИЯ
C*  IDENT  (1) AБCOЛЮTHЫE ИДEHTИФИKATOPЫ
C*  MACRO  (2) MACRO+OPSYN+PURGE
C*
C
 GSTEXT*:,ENTRY,
 GENSTEXT:,XTA,FLGEX*
 ,AOX,DEBUG
 13,U1A,
 ,XTA,STEXTPNT
 ,ATX,ENTRY*
 ,ITA,13
 ,XTS,
 ,ATX,ENTRY*+1
 ,ATX,ENTRY*+2
 14,VTM,-9
 GENSST:14,ATX,STROKA+9
 14,VLM,GENSST
 13,VJM,INITLIB
 ,XTA,TESTST
 13,VJM,WTOLIB
 ,XTA,SAVEDAT
 13,VJM,WTOLIB
 7,VTM,STEXTCOM
 GENST1:7,XTA,
 13,VJM,WTOLIB
 7,XTA,
 ,AAX,RCON377
 ,UZA,GENHEAD
 7,VLM,GENST1
 GENHEAD:,BSS,
 ,ATX,STEXTCNT
 ,ATX,STEXTPNT
 7,UTM,-STEXTCOM+3
 ,ITA,7
 ,ATX,STROKA+7
 ,XTA,RESETFL
 ,UZA,GENIDENT
 13,VJM,GENSWORD
 1,WTC,-2
 ,XTA,.PNKOPTAB
 ,ATI,14
 ,ASN,64+24
 ,ATI,7
 ,ITA,14
 ,ATX,RR1
 7,MTJ,5
 GENMAC:5,XTA,
 ,UZA,GENMAC2
 ,ARX,
 ,U1A,GENMAC3
 GENMAC1:5,WTC,1
 5,VTM,
 5,V1M,GENMAC
 GENMAC2:7,UTM,2
 ,ITA,7
  ,AEX,RR1
 7,MTJ,5
 ,U1A,GENMAC
 ,XTA,RCON2
 13,VTM,GENIDENT
 ,UJ,GENGROOP
 GENMAC3:5,XTA,1
 ,AAX,CON001
 ,UZA,GENMAC1
 5,XTA,1
 ,ASN,64+24
 ,AAX,CONR15
 ,ATX,RR2
 ,ATI,14
 ,ATI,9
 14,XTA,
 1,AEX,-1.JMPMAC*
 ,ASN,64+24
 ,U1A,GENOPS
 5,XTA,
 13,VJM,GENSWORD
 9,XTA,
 ,AAX,CONR12
 ,AOX,CON4 .MACRO FLAG
 13,VJM,GENSWORD
 :9,UTM,1
 9,XTA,
 ,AEX,RCON1
 ,UZA,GENMAC4
 9,XTA,
 ,UJ,GENSWORD
 GENMAC4:9,XTA,
 13,VTM,GENMAC1
 ,UJ,GENSWORD
 GENOPS:1,MTJ,9.PROCTAB
 14,VTM,-1
 GENOPS0:9,XTA,
 ,UZA,GENMAC1
 ,ASN,64+24
 ,AEX,RR2
 14,UTM,1
 ,UZA,GENOPS1
 9,XTA,
 ,AEX,RR2
 ,AAX,CONR24
 14,UTM,1
 ,UZA,GENOPS1
 9,VLM,GENOPS0
 GENOPS1:,ITA,14
 ,ASN,64-24
 5,XTS,1
 ,AAX,CONKP1
 15,AOX,
 ,ATX,RR2
 ,UZA,GENPUR
 5,XTA,
 13,VJM,GENSWORD
 ,XTA,RR2
 13,VTM,GENMAC1
 ,UJ,GENSWORD
 GENPUR:5,XTA,
 ,AOX,CON4
 13,VTM,GENMAC1
 ,UJ,GENSWORD
 GENIDENT:,BSS,
 1,VTM,SECTOR
 13,VJM,GENSWORD
 5,VTM,1-2048
 GENID1:1,XTA,
 ,AEX,T*ЬЬЬЬ
 ,AAX,AA6SIMB
 ,UZA,GENID2
 1,XTA,1
 ,AAX,SYSMASK
 ,AEX,CN0004
 ,UZA,GENIDABS
 ,AEX,CN00001
 ,UZA,GENIDREL
 GENID2:1,UTM,2
 5,VLM,GENID1
 ,XTA,RCON1
 13,VJM,GENGROOP
 GENGLOB:,XTA,GLOB*
 ,UZA,GENSEND
 ,ASN,64-24
 13,VJM,WTOLIB
 ,XTA,STROKA+7
 ,ARX,RCON1
 ,ATX,STROKA+7
 GENSEND:,XTA,RCON1
 ,ATX,STROKA
 ,XTA,ENTRY*
 13,VJM,WTOLIB
 15,WTC,
 13,VTM,
 ,UJ,STOLIB
 GENIDREL:1,XTA,1
 ,ASN,64+24
 ,ARX,CONR15
 ,AEX,CONR15
 1,ARX,1
 ,UJ,GENIDABS+1
 GENIDABS:1,XTA,1
 ,UTC,
 :,AAX,CONR15
 1,XTS,
 ,AAX,CONR18
 ,U1A,GENIDLON
 1,XTA,
 15,AEX,
 :13,VTM,GENID2
 ,UJ,GENSWORD
 GENIDLON:1,XTA,
 13,VJM,GENSWORD
 :15,XTA,
 ,UJ,GENIDLON-1
 GENSWORD:,XTS,STEXTPNT
 ,UZA,GENWORD
 ,XTA,STEXTCNT
 ,ARX,RCON1
 ,STX,STEXTCNT
 ,UJ,WTOLIB
 GENWORD:,XTA,TOLIB1
 ,ATX,STEXTPNT
 ,XTA,
 ,STX,STEXTCNT
 ,UJ,WTOLIB
 GENGROOP:,XTS,TOLIB1
 ,AEX,STEXTPNT
 ,AAX,MASK38
 ,UZA,GENGRP1
 ,XTA,TOLIB1
 12,VJM,EXCHLIB
 ,XTA,STEXTPNT
 ,AOX,CN0001
 12,VJM,EXCHLIB
 GENGRP1:,BSS,
 ,XTA,STEXTPNT
 ,AAX,RMASK10
 ,STI,14
 ,ASN,64-15
 ,AOX,STEXTCNT
 ,UTC,BUFPER
 14,ATX,
 ,XTA,TOLIB1
 ,AEX,STEXTPNT
 ,AAX,MASK38
 ,UZA,GENGRP2
 ,XTA,STEXTPNT
 12,VJM,EXCHLIB
 ,XTA,TOLIB1
 ,AOX,CN0001
 12,VJM,EXCHLIB
 GENGRP2:,XTA,
 ,ATX,STEXTPNT
 ,XTA,STEXTCNT
 ,ARX,STROKA+7
 ,ARX,RCON1
 ,ATX,STROKA+7
 13,UJ,
   1,SET,CONSS
 4096,,SECTOR
   ,END,
