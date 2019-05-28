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
 MADLEN*3:,NAME,
 ,TITLE,ACCEMБЛEP MADLEN. OCHOBHOЙ MOДYЛЬ. ГEHEPATOP KOДOB.
C
C   OCHOBHOЙ MOДYЛЬ ACCEMБЛEPA MADLEN.
C   BEPCИЯ 2: E.Д.ФEДЮHЬKИH  02/06/77.
C   BEPCИЯ 3: M.Ю.ПOПOB, E.Д.ФEДЮHЬKИH
C                            23/06/79.
C
C   OБ'EДИHEHHЫЙ ИHCTИTYT ЯДEPHЫX
C   ИCCЛEДOBAHИЙ (ДYБHA).
C
C
C *************************************
C *                                   *
C *       LACIATE OGNI SPERANZA       *
C *          VOI CH'ENTRARE           *
C *                                   *
C *************************************
C
C
 FOMA***:,SUBP,
 PRIMADL*:,EQU,FOMA***
 TIME:,EQU,PRIMADL*
 BSSPRINT:,EQU,PRIMADL*+1
 COMCNPR: ,EQU,PRIMADL*+2
 COMPRINT:,EQU,PRIMADL*+3
 COMPRI:  ,EQU,PRIMADL*+4
 CONSTPRI:,EQU,PRIMADL*+5
 DECPRINT:,EQU,PRIMADL*+6
 ENDPRO1:,EQU,PRIMADL*+7
 SKIPPR:  ,EQU,PRIMADL*+8
 GERPRI:,EQU,PRIMADL*+9
 SKIPREE:,EQU,PRIMADL*+10
 FATERR:  ,EQU,PRIMADL*+11
 MADNAME:,EQU,PRIMADL*+16
 NODEC:   ,EQU,PRIMADL*+17
 TWODEC:  ,EQU,PRIMADL*+18
 NOENDOP: ,EQU,PRIMADL*+19
 NOOPRI:  ,EQU,PRIMADL*+20
 TOPRINT: ,EQU,PRIMADL*+21
 TOPRINT1:,EQU,PRIMADL*+22
 SETPRI:,EQU,PRIMADL*+23
 IFPR:,EQU,PRIMADL*+30B
 ERIFPR:,EQU,PRIMADL*+31B
 ERRORPR:,EQU,PRIMADL*+32B
 LISTPR:,EQU,PRIMADL*+33B
 CPRINT:,EQU,PRIMADL*+34B
 EJECT:,EQU,PRIMADL*+35B
 SPACE:,EQU,PRIMADL*+36B
 ZEROM:,EQU,PRIMADL*+37B
 LONGM:,EQU,PRIMADL*+40B
 LOCKNM  :,EQU,PRIMADL*+41B
 TWOCOP:,EQU,PRIMADL*+42B
 NOABSI:,EQU,PRIMADL*+43B
 NODEFI:,EQU,PRIMADL*+44B
 NOABSAD:,EQU,PRIMADL*+45B
 NODEFAD:,EQU,PRIMADL*+46B
 LONGAD:,EQU,PRIMADL*+47B
 BADPAR:,EQU,PRIMADL*+50B
 GLOBER:,EQU,PRIMADL*+51B
C
 LISTS:,LC,5120
 SECTOR:  ,EQU,LISTS
 TABDEC:  ,EQU,LISTS+4096
C
 DISCRIPT:,SUBP,
 SCRACH:,EQU,DISCRIPT+40
 BUFPER:,SUBP,
 MSTACK:,EQU,BUFPER
 PRIBUF*:,EQU,BUFPER+2000B
 INFOR:,EQU,BUFPER+4000B
 STROKA:,EQU,INFOR+10
C
 /MACPAR/:,SUBP,
 DPARAM:,EQU,/MACPAR/-STROKA
C
 SAVE*:,SUBP,
C
 READ*:,SUBP,
C
 ENTRY*:,LC,42
C
 MDL*REF:,SUBP,
 MDL*6T8W:,SUBP,
 MDL*6T8S:,SUBP,
C
 MONCARD*:,LC,14
C
 SPASAN*: ,LC,220
 SPAS1:   ,EQU,SPASAN*+22
 SPAS2:   ,EQU,SPASAN*+37
 KART:    ,EQU,SPASAN*+52
 KARTET:  ,EQU,SPASAN*+136
C
 *PUNCHER:,SUBP, . PUNCH STA.
 INITLIB:,SUBP, . INIT. TEMPO. LIBRARY
 WTOLIB :,SUBP, . WRITE TO LIBRARY
 STOLIB :,SUBP, . WRITE CATALOG
 QRESET:,SUBP, . RESET TABLES
 GSTEXT*:,SUBP,
C
 ERROFLG*:,LC,1
C
 PRINT8**:,SUBP,
C
 PRINT8:,SUBP,
C
 RR1:,EQU,14
 RR2:,EQU,15
 RR3:,EQU,16
 MASK24:,EQU,17
 C/5:,EQU,18
 C//5:,EQU,19
 C4:      ,EQU,20
 NSTR:    ,EQU,21
 MACN:,EQU,24
 MRAC:,EQU,26
 INDTABL: ,EQU,27
 BSSCNT:  ,EQU,38
 DATCNT:  ,EQU,39
 FIXCNT:  ,EQU,40
 FLAGTABL:,EQU,41
 ILAST:   ,EQU,42
 CON61:,EQU,43
 KOSCNT:  ,EQU,44
 RCON12:,EQU,45
 CON66:,EQU,46
 CON005:,EQU,47
 CON041:,EQU,48
 CON042:,EQU,49
 CON043:,EQU,50
 CON045:,EQU,51
 NK:      ,EQU,52
 NKABS:   ,EQU,53
 NKCARD:  ,EQU,54
 NKKOS:   ,EQU,55
 POSIND:  ,EQU,56
 RMIST:   ,EQU,57
 MSK370*5:,EQU,58
 MSK0*5:,EQU,59
 MSK7*5:,EQU,60
 CON063:,EQU,61
 SETCNT:  ,EQU,62
 CON066:,EQU,63
 CON076:,EQU,64
 YSLNYL:  ,EQU,65
 *AB*TAB:,EQU,66
 CON306:,EQU,72
 AB:,EQU,73
 AR:      ,EQU,74
 AH:      ,EQU,75
 CON1:    ,EQU,76
 CON2:    ,EQU,77
 CON3:    ,EQU,78
 CON4:    ,EQU,79
 CON6:    ,EQU,80
 CON7:    ,EQU,81
 CON01:   ,EQU,82
 CON02:   ,EQU,83
 CON04:   ,EQU,84
 MASK33:,EQU,85
 CON72:   ,EQU,86
 CON001:  ,EQU,87
 CON002:  ,EQU,88
 CN0001:  ,EQU,89
 CN0002:  ,EQU,90
 CN0004:  ,EQU,91
 MASK37:,EQU,92
 CN00001: ,EQU,93
 CN00002: ,EQU,94
 CN00003: ,EQU,95
 CN0005:,EQU,96
 CN00005: ,EQU,97
 CN00006: ,EQU,98
 CN00007: ,EQU,99
 MASK41:,EQU,100
 CN000001:,EQU,101
 CN000002:,EQU,102
 CN000003:,EQU,103
 TOP:,EQU,104
 AA6SIMB: ,EQU,105
 AA6SIMBR:,EQU,106
 BSIMB:   ,EQU,107
 RCON1:   ,EQU,108
 CONR15:  ,EQU,109
 MB40:,EQU,110
 CNR4000: ,EQU,111
 CNR7S12:,EQU,112
 CONSS:   ,EQU,113
 FLAG3B:  ,EQU,114
 RCON320:,EQU,115
 RCON20:  ,EQU,116
 TOP1:,EQU,117
 RCON70:  ,EQU,118
 CON004:  ,EQU,119
 RCN77766:,EQU,120
 ERCNT:   ,EQU,121
 CONR24:  ,EQU,122
 CONR12:  ,EQU,123
 CREAL10: ,EQU,124
 ZMPAR:,EQU,125
 T*ЬЬЬЬ:,EQU,126
 CONKP1:  ,EQU,127
 RCONY:   ,EQU,128
 NKBSS:   ,EQU,129
 NKDATA:  ,EQU,130
 MARDIS:,EQU,131
 KBSSAD:  ,EQU,132
 CON33:,EQU,133
 CON776:  ,EQU,134
 COMMA:,EQU,135
 CONDLI1: ,EQU,136
 CON43:,EQU,137
 RBLANK4: ,EQU,138
 CINT0:   ,EQU,139
 C701S6:,EQU,140
 CON/6:,EQU,141
 DEBUG:,EQU,142
 DEBUGF:,EQU,143
 BASKOP:,EQU,144
 OUTMADLE:,EQU,145
 MESREAD*:,EQU,146
 MWCNT:,EQU,147
 VERETTI: ,EQU,148
 BASEMOD: ,EQU,149
 BASE*:   ,EQU,150
 BASIR:   ,EQU,151
 BASPR:   ,EQU,152
 UTCPR:   ,EQU,153
 UTCREG:  ,EQU,154
 GLOKOP:  ,EQU,155
 YSLUTC:,EQU,156
 BASYPR:,EQU,157
 BASLONG: ,EQU,158
 UTCCONST:,EQU,159
 XBASE:   ,EQU,160
 GLOB*:   ,EQU,161
 LISMASK0:,EQU,162
 BLBSS:   ,EQU,163
 BLUTC:   ,EQU,164
 VERET:   ,EQU,165
 CON104:,EQU,166
 BASYSL:  ,EQU,167
 NO*DEC:  ,EQU,168
 SOS:     ,EQU,169
 NN:,EQU,170
 IDFLAG:,EQU,171
 IFFLAG:,EQU,172
 XTEXT:   ,EQU,173
 LISTF:   ,EQU,174
 EDITF:   ,EQU,175
 MODEL:   ,EQU,176
 CON774:,EQU,181
 DDT:     ,EQU,182
 INF*70:,EQU,183
 RESETFL:,EQU,184
 STEXTPNT:,EQU,185
 STEXTCNT:,EQU,186
 TITLE1:  ,EQU,187
 DATE:    ,EQU,192
 TIMEF:   ,EQU,194
 PAGE:    ,EQU,196
 TITLE2:  ,EQU,198
 MADVERS: ,EQU,202
 CONR17:,EQU,209
 IFNAM:   ,EQU,210
 HASH:    ,EQU,211
 PACKH:   ,EQU,212
 CUTH:    ,EQU,213
 LONGSTR:,EQU,214
 NG1B:    ,EQU,215
 NGZB:    ,EQU,216
 RCON17:,EQU,217
 CON534:,EQU,218
 IFADRF:  ,EQU,219
 EXPO**:,EQU,220
 I1:,EQU,221
 C13S15:,EQU,233
 *PROBEL:,EQU,234
 C52104S9:,EQU,235
 REN*FL:,EQU,236
 C403S15:,EQU,237
 SAVEDAT:,EQU,238
 FLGEX*:,EQU,239
 REISRR1:,EQU,240
 REISSYM:,EQU,241
 REISRR2:,EQU,242
 REISRR4:,EQU,243
 REISRR5:,EQU,244
 REISCS:,EQU,245
 RE*R:,EQU,246
 RE*RAB:,EQU,247
 RE*RAB1:,EQU,248
 RE*RTLG:,EQU,249
 RE*RREG:,EQU,250
 TELLI:,EQU,251
 IMIST:,EQU,252
 TSCF:,EQU,253
 LEFINST:,EQU,254
 BLDEF:,EQU,255
 BLDEFB:,EQU,256
 R1:,EQU,260
 IRS:,EQU,261
 IRB:,EQU,262
 IRT:,EQU,263
 IRSS:,EQU,264
 IRE:,EQU,265
 IRSH:,EQU,266
 IRPF:,EQU,267
 IRP:,EQU,268
 IRFLAG:,EQU,269
 MSTR:,EQU,270
 NCON01:,EQU,279
 BLUTC1:,EQU,280
 NGRSS:,EQU,281
 FLAGPRO:,EQU,282
 MESADR5:,EQU,283
 XADRESS5:,EQU,284
 MESADRB:,EQU,285
 XADRESSB:,EQU,286
 BASE**:,EQU,287
 MESADR1:,EQU,288
 MESADR2:,EQU,289
 XADRESS1:,EQU,290
 XADRESS2:,EQU,291
 MESKOP:,EQU,292
 MESMET:,EQU,293
 MESMOD:,EQU,294
 MISTABL:,EQU,295
 IFCNTM:,EQU,296
 IFCNT:,EQU,297
 MESADR4:,EQU,298
 OCTAL:,EQU,299
 DECIMAL:,EQU,300
 SIGN:,EQU,301
 SIGNS:,EQU,302
 SIGNE:,EQU,303
 SIGN1:,EQU,304
 ASTMET:,EQU,305
 NUMMET:,EQU,306
 ASTNUMET:,EQU,307
 MESADR3:,EQU,308
 A8:,EQU,309
 LISTECK:,EQU,310
 Q*1RENS:,EQU,326
 Q*2RENS:,EQU,327
 Q*3RENS:,EQU,328
 STN1:,EQU,329
 ZR1:,EQU,330
 ZR2:,EQU,331
 ZR3:,EQU,332
 C201S15:,EQU,333
 CN00014:,EQU,334
 CN00015:,EQU,335
 CN00016:,EQU,336
 CN00017:,EQU,337
 NCON6:,EQU,338
 NCON7:,EQU,339
 CN000044:,EQU,340
 CN000054:,EQU,341
 CN000064:,EQU,342
 CN000074:,EQU,343
 CN000102:,EQU,344
 MASK*06:,EQU,345
 MASK*07:,EQU,346
 MASK*17:,EQU,347
 MASK*20:,EQU,348
 MASK*40:,EQU,349
 MASK*50:,EQU,350
 MASK*60:,EQU,351
 MASK*70:,EQU,352
 EJTEXT:,EQU,353
 NCON04:,EQU,354
 NCN0006:,EQU,355
 NCON76:,EQU,356
 NCON77:,EQU,357
 M47S15:,EQU,358
 M33S47:,EQU,359
 RMASK11:,EQU,360
 RMASK14:,EQU,361
 RCON5:,EQU,362
 RCON7:,EQU,363
 RCON15:,EQU,364
 RCON150:,EQU,365
 OCT*PLG:,EQU,366
 CONSS2:,EQU,367
 CONSS3:,EQU,368
 CONRV:,EQU,369
 CONW:,EQU,370
 CONBAS:,EQU,371
 CONNG:,EQU,372
 CONRL:,EQU,373
 CONCL:,EQU,374
 CON*T:,EQU,375
 CONG:,EQU,376
 MODTEXT:,EQU,377
 ABSCNT:,EQU,378
 MACON:,EQU,379
 CNR10000:,EQU,380
 AB*PLG:,EQU,381
 RDLITS:,EQU,382
 DEBMASK:,EQU,383
 CONRE81:,EQU,384
 CONRE84:,EQU,385
 CONSUB:,EQU,386
 CONSB:,EQU,387
 CONISK:,EQU,388
 LIS*BV:,EQU,389
 LIS*LR:,EQU,390
 NLIS*BRV:,EQU,391
 SSW:,EQU,392
 RCON200:,EQU,397
 RCON377:,EQU,398
 R3B:,EQU,399
 SKIPFLAG:,EQU,400
 LISTAND:,EQU,401
 LISMASK:,EQU,402
 RCON100:,EQU,403
 LISMAD:,EQU,404
 LISLAST:,EQU,405
 LIS*G:,EQU,406
 LIS*B:,EQU,407
 LIS*C:,EQU,408
 LIS*E:,EQU,409
 LIS*F:,EQU,410
 LIS*L:,EQU,411
 LIS*R:,EQU,412
 LIS*S:,EQU,413
 LIS*V:,EQU,414
 RDFLAG:,EQU,415
 RCON6:   ,EQU,416
 RCON60:  ,EQU,417
 *EQPLG:,EQU,418
 *KOPPLG:,EQU,419
 CONR16:,EQU,420
 NKMAX:,EQU,421
 NCON1:,EQU,422
 RCON50:,EQU,423
 CNR104S9:,EQU,424
 CNR304S9:,EQU,425
 CODPLG:,EQU,426
 CON*31:,EQU,427
 GLOBCON:,EQU,428
 NRC14:,EQU,429
 MODMASK:,EQU,430
 MASK44:,EQU,431
 PSKPLG:,EQU,432
 IFPLG:,EQU,433
 I:,EQU,434
 I2:,EQU,456
 NOLMASK:,EQU,472
 CSSILK:,EQU,473
 CON44:,EQU,474
 TLREG:,EQU,475
 TLREGC:,EQU,478
 TLREGR:,EQU,479
 TLREGL:,EQU,480
 TL0:,EQU,481
 TLZ:,EQU,482
 TLGRYS:,EQU,483
 RCON40:,EQU,495
 RCON340:,EQU,496
 RCON10:,EQU,497
 TESTST:,EQU,498
 CN000007:,EQU,499
 MASK30:,EQU,500
 NCON2:,EQU,501
 NCON3:,EQU,502
 NCON4:,EQU,503
 LIS*D:,EQU,504
 *COMPLG:,EQU,505
 FLGADR:,EQU,506
 ENDM*S:,EQU,507
 CN006:,EQU, 509
 ZISO0:,EQU,510
 ZGOST7:,EQU,511
 RCON2:,EQU,512
 CON74:,EQU,513
 EXT:,EQU,514
 EXTMAX:,EQU,515
 GCNT:    ,EQU,516
 LCNT:    ,EQU,517
 MSK*200: ,EQU,518
 MSK*177: ,EQU,519
 MSK*012: ,EQU,520
 EXTFLG:,EQU,521
 MACS5*:,EQU,522
 MACE5*:,EQU,523
 *ЬЬЬЬ:,EQU,524
 MAINPRIN:,EQU,525
 CON777:,EQU,526
 LIS*MS:,EQU,527
 ZIRP:,EQU,528
 IRPF1:,EQU,529
 IRPF2:,EQU,530
 IRPF3:,EQU,531
 IRPF4:,EQU,532
 LOCTEXT:,EQU,544
 RENKL:  ,EQU,550
 CON0004 :,EQU,567
 CON0002 :,EQU,568
 CON0001 :,EQU,569
 CON00004:,EQU,570
 CON00002:,EQU,571
 CON00001:,EQU,572
 BADPARAM:,EQU,573
 MASK38  :,EQU,579
 CON5:,EQU,580
 STEXTCOM:,EQU,581
 STNAM*:,EQU,587
 RCON3:,EQU,595
 FLFAT:,EQU,596
 RSTNR:,EQU,597
 RISO10:,EQU,598
 D4B:,EQU,599
 STAR3B:,EQU,600
 CONPRIL:,EQU,601
 RMASK26:,EQU,602
 C/10:,EQU,603
 CTM:,EQU,604
 MDNS*1:,EQU,620
 MDNS*2:,EQU,621
 SYSABS:,EQU,622
 SYSREL:,EQU,623
 SYSMASK:,EQU,624
 CN000004:,EQU,625
 NCARD:,EQU,626
 CON76:,EQU,627
 SAVI15:,EQU,628
 AE:,EQU,629
 CONR18:,EQU,630
 CONDG:,EQU,631
C
C
 MACSYV:,EQU,137B
 MACSYM:,EQU,65
C******  OPTIONED BY COSTUMERS  *****
 MACSYV1:,EQU,137B
 MACSYM1:,EQU,65
C
 CNR40000:,EQU,LIS*R
 CN00004:,EQU,LIS*L
 LIS*M:,EQU,CN00002
 LIS*N:,EQU,CN00001
 LIS*T:,EQU,CNR10000
 LIS*U:,EQU,CNR4000
 MULTI:,EQU,STROKA+INDTABL
C
C
C**************************************
C*
C*   BXOДЫ ДЛЯ ИHИЦИATOPA.
C*
C
 :,UJ,*FNAME
 SPY:1,VTM,PROCTAB
 3,VTM,HASHKOP
 13,UJ,
 RUN:6,VTM,STROKA
 13,VJM,TIME
 :,XTA,
 13,VJM,SAVE*
 :,XTA,
 ,ATX,ENTRY*
 1,VTM,PER
 13,VJM,*READ4
 13,VJM,MACSTECK
 :13,VTM,*ABP1
 ,UJ,*WRI
C
C**************************************
C*
C*  ДИHAMИЧECKИЙ ПEPEKЛЮЧATEЛЬ CИMBOЛOB
C*  HYЛEBOГO YPOBHЯ. ГЛABHЫЙ BXOД.
C*
C
 ABP:13,VJM,READA
 :14,WTC,*+1
             ,  UJ   ,                    HA ДИHAMИЧECKИЙ ПEPEKЛЮЧATEЛЬ
      :,,
 ,UJ,ERSIM                                000
      :,,
 ,UJ,ERSIM                                001
      :,,
 ,UJ,ERSIM                                002
      :,,
 ,UJ,ERSIM                                003
      :,,
 ,UJ,ERSIM                                004
      :,,
 ,UJ,ERSIM                                005
      :,,
 ,UJ,ERSIM                                006
      :,,
 ,UJ,ERSIM                                007
      :,,
 ,UJ,ERSIM                                010
      :,,
 ,UJ,ERSIM                                011
      :,,
 ,UJ,ERSIM                                012
      :,,
 ,UJ,ERSIM                                013
      :,,
 ,UJ,ERSIM                                014
      :,,
 ,UJ,ERSIM                                015
      :,,
 ,UJ,ERSIM                                016
      :,,
 ,UJ,ERSIM                                017
      :,,
 ,UJ,ERSIM                                020
      :,,
 ,UJ,ERSIM                                021
      :,,
 ,UJ,ERSIM                                022
      :,,
 ,UJ,ERSIM                                023
      :,,
 ,UJ,ERSIM                                024
      :,,
 ,UJ,ERSIM                                025
      :,,
 ,UJ,ERSIM                                026
      :,,
 ,UJ,ERSIM                                027
      :,,
 ,UJ,ERSIM                                030
      :,,
 ,UJ,ERSIM                                031
      :,,
 ,UJ,ERSIM                                032
      :,,
 ,UJ,ERSIM                                033
      :,,
 ,UJ,ERSIM                                034
      :,,
 ,UJ,ERSIM                                035
 PER     :   ,       ,
 ,UJ,ERSIM                                036
      :,,
 ,UJ,ERSIM                                037
      :,,
 ,UJ,ABP.ПPOБEЛ
             ,       ,
             ,  UJ   ,ERSIM                                 3
             ,       ,
             ,  UJ   ,ERSIM               ''                4
             ,       ,
             ,  UJ   ,ERSIM                                 5
             ,       ,
             ,  UJ   ,ERSIM                                 6
             ,       ,
             ,  UJ   ,ERSIM                                 7
             ,       ,
             ,  UJ   ,ERSIM                                 8
             ,       ,
             ,  UJ   ,APOST               '                 9
             ,       ,
             ,  UJ   ,ERSIM               (                10
             ,       ,
             ,  UJ   ,ERSIM               )                11
         : 12,  VTM  ,10
             ,  UJ   ,WRITE               *                12
           12,  VTM  ,30
             ,  UJ   ,ERSIM                                13
             ,       ,
             ,  UJ   ,POSMOD              ,                14
         : 12,  VTM  ,29
             ,  UJ   ,ERSIM               -                15
             ,       ,
             ,  UJ   ,EDITTT              .                16
         : 12,  VTM  ,15
             ,  UJ   ,WRITE               /                17
           12,  VTM  ,16
             ,  UJ   ,WRITE               0                18
           12,  VTM  ,17
             ,  UJ   ,WRITE               1                19
           12,  VTM  ,18
             ,  UJ   ,WRITE               2                20
           12,  VTM  ,19
             ,  UJ   ,WRITE               3                21
           12,  VTM  ,20
             ,  UJ   ,WRITE               4                22
           12,  VTM  ,21
             ,  UJ   ,WRITE               5                23
           12,  VTM  ,22
             ,  UJ   ,WRITE               6                24
           12,  VTM  ,23
             ,  UJ   ,WRITE               7                25
           12,  VTM  ,24
             ,  UJ   ,WRITE               8                26
           12,  VTM  ,25
             ,  UJ   ,WRITE               9                27
             ,       ,
             ,  UJ   ,POSMET              :                28
             ,       ,
             ,  UJ   ,ERSIM               )                29
             ,       ,
             ,  UJ   ,ERSIM               Л                30
             ,       ,
             ,  UJ   ,ERSIM               =                31
             ,       ,
             ,  UJ   ,ERSIM               >                32
             ,       ,
             ,  UJ   ,ERSIM                                33
             ,       ,
             ,  UJ   ,ERSIM                                34
         : 12,  VTM  ,33
             ,  UJ   ,WRITE               A                35
           12,  VTM  ,34
             ,  UJ   ,WRITE               B                36
           12,  VTM  ,35
             ,  UJ   ,WRITE               C                37
           12,  VTM  ,36
             ,  UJ   ,WRITE               D                38
           12,  VTM  ,37
             ,  UJ   ,WRITE               E                39
           12,  VTM  ,38
             ,  UJ   ,WRITE               F                40
           12,  VTM  ,39
             ,  UJ   ,WRITE               G                41
           12,  VTM  ,40
             ,  UJ   ,WRITE               H                42
           12,  VTM  ,41
             ,  UJ   ,WRITE               I                43
           12,  VTM  ,42
             ,  UJ   ,WRITE               J                44
           12,  VTM  ,43
             ,  UJ   ,WRITE               K                45
           12,  VTM  ,44
             ,  UJ   ,WRITE               L                46
           12,  VTM  ,45
             ,  UJ   ,WRITE               M                47
           12,  VTM  ,46
             ,  UJ   ,WRITE               N                48
           12,  VTM  ,47
             ,  UJ   ,WRITE               O                49
           12,  VTM  ,48
             ,  UJ   ,WRITE               P                50
           12,  VTM  ,49
             ,  UJ   ,WRITE               Q                51
           12,  VTM  ,50
             ,  UJ   ,WRITE               R                52
           12,  VTM  ,51
             ,  UJ   ,WRITE               S                53
           12,  VTM  ,52
             ,  UJ   ,WRITE               T                54
           12,  VTM  ,53
             ,  UJ   ,WRITE               U                55
           12,  VTM  ,54
             ,  UJ   ,WRITE               V                56
           12,  VTM  ,55
             ,  UJ   ,WRITE               W                57
           12,  VTM  ,56
             ,  UJ   ,WRITE               X                58
           12,  VTM  ,57
             ,  UJ   ,WRITE               Y                59
           12,  VTM  ,58
             ,  UJ   ,WRITE               Z                60
             ,       ,
             ,  UJ   ,ERSIM               Г                61
             ,       ,
             ,  UJ   ,ERSIM                                62
             ,       ,
             ,  UJ   ,ERSIM               J                63
             ,       ,
             ,  UJ   ,ERSIM                                64
             ,       ,
 ,UJ,ABP.UNDERLINE     65
         : 12,  VTM  ,63
             ,  UJ   ,WRITE               Ю                66
             ,       ,
             ,  UJ   ,ERSIM                                67
         : 12,  VTM  ,2
             ,  UJ   ,WRITE               Б                68
           12,  VTM  ,3
             ,  UJ   ,WRITE               Ц                69
           12,  VTM  ,4
             ,  UJ   ,WRITE               Д                70
             ,       ,
             ,  UJ   ,ERSIM                                71
         : 12,  VTM  ,5
             ,  UJ   ,WRITE               Ф                72
           12,  VTM  ,6
             ,  UJ   ,WRITE               Г                73
             ,       ,
             ,  UJ   ,ERSIM                                74
         : 12,  VTM  ,7
             ,  UJ   ,WRITE               И                75
           12,  VTM  ,11
             ,  UJ   ,WRITE               Й                76
             ,       ,
             ,  UJ   ,ERSIM                                77
         : 12,  VTM  ,12
             ,  UJ   ,WRITE               Л                78
             ,       ,
             ,  UJ   ,ERSIM                                79
             ,       ,
             ,  UJ   ,ERSIM                                80
             ,       ,
             ,  UJ   ,ERSIM                                81
           12,  VTM  ,28
             ,  UJ   ,WRITE               П                82
         : 12,  VTM  ,13
             ,  UJ   ,WRITE               Я                83
             ,       ,
             ,  UJ   ,ERSIM                                84
             ,       ,
             ,  UJ   ,ERSIM                                86
             ,       ,
             ,  UJ   ,ERSIM                                85
             ,       ,
             ,  UJ   ,ERSIM                                87
         : 12,  VTM  ,14
             ,  UJ   ,WRITE               Ж                88
             ,       ,
             ,  UJ   ,ERSIM                                89
         : 12,  VTM  ,26
             ,  UJ   ,WRITE               Ь                90
           12,  VTM  ,31
             ,  UJ   ,WRITE               Ы                91
           12,  VTM  ,32
             ,  UJ   ,WRITE               З                92
           12,  VTM  ,59
             ,  UJ   ,WRITE               Ш                93
           12,  VTM  ,60
             ,  UJ   ,WRITE               Э                94
           12,  VTM  ,61
             ,  UJ   ,WRITE               Щ                95
           12,  VTM  ,62
             ,  UJ   ,WRITE               Ч                96
             ,       ,
             ,  UJ   ,ERUTC                                97
      /1/:,,
             ,  UJ   ,POSMET              ДЛЯ BOCCTAHOBЛE- 98
      /2/:12,VTM,30
             ,  UJ   ,WRITE               HИЯ ПEPEKЛЮЧATEЛЯ99
      /3/:12,VTM,29
             ,  UJ   ,WRITE                               100
      /4/:12,VTM,15
             ,  UJ   ,WRITE                               101
 /5/:,,
             ,  UJ   ,APOSKOP                             102
      /6/:12,VTM,27
             ,  UJ   ,EDITTT                              103
 /RSHIFT:,,
 ,UJ,RHSIFT
      /8/:,,
             ,  UJ   ,APOST                               105
      /9/:,,
             ,  UJ   ,ERUTC                               106
 /10/:,,
             ,  UJ   ,POSMOD                              107
 /11/:12,VTM,30
 ,UJ,ERSIM1
 /12/:12,VTM,29
 ,UJ,ERSIM1
 PER*1:,EQU,PER-1
 PER*3:,EQU,PER-3
C
C**************************************
C*
C*  ЧTEHИE OЧEPEДHOЙ KAPTЫ, AHAЛИЗ
C*  YПPABЛЯЮЩEГO CИMBOЛA.
C*
C
 ABP1:,UJ,*ABP1
 ,UTC,
 13,VTM,*ABP1
 ,UZA,LITWRITE
 *ABP1   :  6,  XTA  ,NKCARD
            6,  ARX  ,RCON1               NKCARD=NKCARD+1
            6,  ATX  ,NKCARD
 6,XTA,AE
 6,ATX,FLAGPRO
 6,XTA,*PROBEL
 6,ATX,RDFLAG
 ,ITA,
 6,ATX,VERET
 6,ATX,MISTABL
 6,ATX,MESMOD
 6,ATX,MESADR5
 6,ATX,XADRESS5
 6,ATX,EXPO**
 1,XTS,C1READ1-PER
 13,VTM,READRET
 READ*S:6,WTC,MESREAD*
 14,VTM,
 14,UTC,
 14,V1M,
 6,XTS,RCON1
 6,ARX,NCARD
 6,STX,NCARD
 ,UJ,READ*
 READRET:6,ATX,AE
 13,VJM,READ
           14,  UTM  ,-32
           14,  VZM  ,*ABP11              YC-ПPOБEЛ
 14,UTM,32-67
 14,VZM,SKITES-1
 14,UTM,67-76
 13,VTM,*ABP11
 14,VZM,RELONG5
 6,XTA,SKIPFLAG
 14,UTM,76-42
 13,VTM,*ABP1A+1
 14,V1M,*AB*4+2
 ,U1A,SKITES+1
 C1READ1:6,XTA,LISMAD
 14,VTM,STROKA
 12,VTM,-5
 ,U1A,*ABP1A+1
 *ABP1*1:14,XTA,
 12,UTC,*AB*TAB+5
 6,AEX,
 12,UZA,*AB*PER+4
 12,VLM,*ABP1*1
 ,UJ,*ABP1A
 :6,XTA,FLAGPRO
 6,ATX,AE
 SKITES:6,XTA,SKIPFLAG
 ,UZA,*ABP1A+1
 6,XTA,LISLAST
 6,AAX,LIS*F
 13,VTM,*ABP1
 ,U1A,*ABP1A+1
 PLUGTSC:,UJ,*READ4
 ,UJ,IF*TSC
 *AB*5:6,AAX,MSK370*5
 6,AEX,MSK0*5
 ,U1A,*ABP1A
 14,XTA,1
 6,APX,MSK7*5
 ,ASN,64+33
 6,ATX,SOS
 ,UJ,*ABP1A+1
 *AB*PER:6,XTA,LIS*L
 ,UJ,*AB*1
 :6,XTA,LIS*L
 ,UJ,*AB*2
 :6,XTA,LIS*L
 ,UJ,*AB*3
 :,UJ,*ABP1A+1
 :6,XTA,LIS*E
 ,UJ,*AB*4
 :14,XTA,1
 ,UJ,*AB*5
 *AB*1:6,AEX,LIS*R
 6,AOX,LISLAST
 6,ATX,LISLAST
 ,UJ,*ABP1A+1
 *AB*2:6,AEX,LIS*R
 ,UJ,*AB*4
 *AB*3:6,AOX,LISLAST
 6,ATX,LISLAST
 6,XTA,LIS*R
 *AB*4:6,AEX,NGZB
 6,AAX,LISLAST
  6,ATX,LISLAST
 ,UJ,*ABP1A+1
 :6,XTA,NKCARD
 ,ASN,64+1
 ,U1A,NOOPRI
 13,VJM,FATERR
 :13,VTM,*FNAME
 ,UJ,NOOPRI
 R*MCNT  :,BSS,
 R*DUP   :,BSS,
 R*MIN:,BSS,
 R*USE   :,BSS,
 R*RMT   :,BSS,
 R*VFD   :,BSS,
 R*CNTRL :,BSS,
 R*ECHO  :,BSS,
 R*IF    :,BSS,
 R*OMIC  :,BSS,
 R*XTEXT :,BSS,
 R*IFC   :,BSS,
 R*DATPR :,BSS,
 R*EDIT  :,BSS,
 R*MAX   :,BSS,
 R*SST   :,BSS,
 R*SSW   :,BSS,
 R*HERE  :,BSS,
 R*PUNCH :,BSS,
 R*MICRO :,BSS,
 R*RJ    :,BSS,
 R*DMIC  :,BSS,
 *ABP1A:13,VJM,NOOPRI
 :6,XTA,AE
 13,VTM,*+5
 ,UZA,*READ4
 6,ARX,CONR15
 6,AAX,CONR15
 6,ATX,AE
 7,VTM,-32
 13,VJM,RELONG5
 13,VTM,LEFCOMT
 ,UJ,CSKIP
 :6,XTA,RCON1
 6,ATX,EXPO**
         : 13,  VTM  ,*ABP1
             ,  UJ   ,DECPRINT
 *ABP11:,XTA,
            6,  ATX  ,IMIST
            6,  ATX  ,MESMET
 6,ATX,ASTNUMET
            6,  ATX  ,INDTABL+1
 6,ATX,*EQPLG
 6,ATX,BLDEF
      1,XTA,/8/-PER
            1,  ATX  ,9
 1,XTA,7
 1,XTA,/11/-PER
 1,ATX,13
 1,XTA,/12/-PER
 1,ATX,15
      1,XTA,/1/-PER
            1,  ATX  ,28
      1,XTA,/9/-PER
            1,  ATX  ,97
      1,XTA,/10/-PER
            1,  ATX  ,14
 STEXTJMP:,UJ,ABP
 ,UJ,R*STEXT
C
C**************************************
C*
C*  HAЧAЛЬHAЯ OБPAБOTKA MHEMOKOДA.
C*
C
 APOSKOP:1,XTA,7
            1,  ATX  ,14
            1,  ATX  ,13
            1,  ATX  ,15
 6,XTA,INDTABL
 6,ATX,MESKOP
 PLUGTS:13,Z31,*WRI
 ,UJ,IF*TS
 :6,XTA,IMIST
             ,  UZA  ,*APKOP
            6,  XTA  ,MISTABL
            6,  AOX  ,CON04               ПPИЗHAK OШИБKИ B MHEMOKOДE
            6,  ATX  ,MISTABL
 6,ATX,BLDEF
 *APKOP:6,XTA,IFPLG
 ,UZA,POSKOP
 ,ATI,14
 14,UJ,
 *APKOP1:6,XTA,IMIST
             ,  U1A  ,FATNAME             OШИБKA B ЗAГOЛOBKE
 6,ATX,IFPLG
 6,ATX,PSKPLG
            6,  XTA  ,MESKOP
 14,VTM,KOPTAB
 14,AEX,NAME*T-KOPTAB
 ,UZA,R*NAME
 6,XTA,MESKOP
 14,AEX,SUBP*T-KOPTAB
 ,UZA,R*SIBESM
 ,UJ,FATNAME
 *CM1:12,VTM,47
 ,UJ,WRITE
 *C:, ,
 ,UJ,ADD
 *C1:, ,
 ,UJ,SUB
 *C2:, ,
 ,UJ,EQU
 *C3:, ,
 ,UJ,APO
 *C4:, ,
 ,UJ,TO4
 *C5:, ,
 ,UJ,TO41
 *C9:, ,
 ,UJ,ETAP2
 *C10:,,
 ,UJ,ETAP3
 *C15:, ,
 ,UJ,RADD
 *C16:, ,
 ,UJ,RSUB
 *C17:, ,
 ,UJ,RTO4
 *C18:, ,
 ,UJ,EQI
 *C19:, ,
 ,UJ,EQR
 *C20:, ,
 ,UJ,EQH
 ENMCON:,Z00,
 ,Z00,3777B-TABDEC
 /2TOЧ:,,
 ,UJ,LLOG
 *C24:12,VTM,60B
 ,UJ,WRITE
 TABADR:,,
 ,Z00,TABDEC
 *C28:12,VTM,37
 ,UJ,WRITE
 ENTRADR:,,
 ,Z00,ENTRY*
 BENTR:,,
 ,Z00,ENTRY*
 ATC:,,
 ,Z00,TABDEC+1
 BBB:,,
 ,Z00,TABDEC+1022
 *C33:12,VTM,41
 ,UJ,WRITE
 *C34:12,VTM,50
 ,UJ,WRITE
 *REIHHH:12,VTM,40
 ,UJ,WRITE
 *REOCTC:, ,
 ,UJ,*REOCT1
 *RELOGC:, ,
 ,UJ,*REOCT2
 /D/:, ,
 ,UJ,DOUBLE
 /D*/:12,VTM,36
 ,UJ,WRITE
 LOGPL:, ,
 ,UJ,*REOCTSH
 :, ,
 ,UJ,*REOCTMI
 :, ,
 ,UJ,*REOCTPL
 :12,VTM,51
 ,UJ,WRITE
 :12,VTM,51
 ,UJ,SHF1
C
C**************************************
C*
C*  OБPAБOTKA HATYPAЛЬHЫX MИKPOCOB.
C*
C
 APOSTLIT:6,STX,-8
            6,  ATX  ,-9
 ,ITA,13
 ,XTS,
 6,ATX,-7
 6,ATX,COMMA
 12,VTM,-16
 13,VJM,READ
 :14,UTM,-177B
 14,VZM,*APLITE
 14,UTM,177B-47B
 14,VZM,*APLIT
 14,UTM,47B-54B
 14,VZM,*APLITC
 14,UTM,54B-60B
             ,  ITA  ,14
            6,  AAX  ,RCON70
 ,U1A,*APLITE
            6,  XTA  ,-7
             ,  ASN  ,64-3
 ,ITS,14
 15,AEX,
            6,  ATX  ,-7
 12,VLM,READ
 *APLITE:,XTA,
 6,STX,COMMA
 6,WTC,-8
 ,UJ,
 *APLITC:6,XTA,CON4
 6,ATX,COMMA
 *APLIT:6,XTA,-7
 6,ASX,-9
 ,U1A,*APLITE
 6,XTA,-7
 15,WTC,
 ,UJ,
 APOST:1,XTA,28
 1,AEX,/1/-PER
 6,AOX,INDTABL
 6,AOX,IMIST
 ,UZA,SKITES
 1,XTA,7
 1,ATX,9
 ,UJ,ERSIM
C
C**************************************
C*
C*  OБPAБOTKA  ИHCTPYKЦИИ  END  .
C*
C
 R*END:13,Z31,LEFCOM
 ,UJ,*APKOP1
 PLG1:13,VTM,*+4
 ,UJ,*READ4
 :13,VTM,ENDMADLE
 6,XTA,NKDATA
 6,ARX,BSSCNT
 6,AAX,CONR15
 6,ATX,MESADR5
 ,UJ,DECPRINT
 :6,XTA,RCON1
 13,VJM,SAVE*
 :1,XTA,R*END-PER
 6,ATX,IFPLG
 ,ITA,
 6,ATX,MRAC
 6,ATX,MESREAD*
 6,ATX,GCNT
 6,ATX,LCNT
 13,VJM,MACSTECK
 6,XTA,RCON1
 6,ATX,EXTFLG
 12,VTM,PNKOPTAB
 6,XTA,RESETFL
 13,VTM,R*END1
 ,U1A,QRESET
 R*END1:15,MTJ,13
 ,CTX,0
 15,VTM,PLAG2+1
 15,XTA,
 1,STX,ABP1-PER
 1,STX,PLG1-PER
 1,STX,PLG23-PER
 1,STX,PLG*LEF-PER
 1,STX,PLG2-PER
 1,STX,PLG3-PER
 1,STX,PLG4-PER
 1,STX,PLG5-PER
 1,STX,*MADEC2-PER
 1,STX,PLG6-PER
 1,STX,PLG7-PER
 1,ATX,PLG8-PER
 ,XTA,
 1,STX,ENDADR5-PER
 1,STX,ENDADR6-PER
 1,STX,PLG9-PER
 1,STX,PLG10-PER
 1,STX,*ETAP3Z4-PER
 1,STX,PLG11-PER
 1,STX,PLG12-PER
 1,ATX,PLG13-PER
 1,XTA,ADR*RA-PER
 1,STX,PLG14-PER
 1,STX,PLG15-PER
 1,STX,PLG16-PER
 1,STX,PLG17-PER
 1,STX,PLG18-PER
 1,STX,PLG19-PER
 1,STX,PLG22-PER
 1,STX,PLGKOP-PER
 1,STX,PLGKOP1-PER
 1,STX,*RENS9-PER
 1,STX,PLG20-PER
 1,STX,PLG21-PER
 1,STX,PLG72-PER
 1,ATX,PLG73-PER
 :6,XTA,DEBUG
 ,UZA,*+2
 1,XTA,*DEBUG-PER
 1,ATX,PLGD-PER
 13,MTJ,15
 ,CTX,1
C
C**************************************
C*
C*  ПPEOБPAЗOBAHИE PAБOЧИX TAБЛИЦ ПEPEД
C*  BTOPЫM ПPOXOДOM (PASS 2).
C*
C
 :,XTA,
 6,ATX,LISTECK
 6,ATX,ILAST
 6,ATX,SYSREL
 6,XTA,CN0004
 6,ATX,SYSABS
 ,NTR,3
 6,XTA,LIS*BV
 6,AAX,LISLAST
 6,XTS,LIS*LR
 6,AAX,LISLAST
 15,ATX,
 ,ASN,64+6
 15,AAX,
 6,AEX,NLIS*BRV
 15,ATX,
 6,AAX,LISMASK
 6,STX,LISMASK
 6,AAX,LISTAND
 15,AEX,
 6,ATX,LISTAND
 6,ATX,LISLAST
 6,XTA,NK
            6,  ARX  ,KOSCNT
            6,  A-X  ,DATCNT
            6,  A-X  ,SETCNT
            6,  A-X  ,BSSCNT
            6,  ATX  ,NKBSS
            6,  ATX  ,NKDATA
            6,  A-X  ,KOSCNT
            6,  ATX  ,NKKOS
 6,ATX,RR2
            6,  A-X  ,ABSCNT
            6,  ATX  ,NKABS
 6,XTA,KOSCNT
 14,VTM,ENTRY*
 12,VTM,SECTOR-2
 3,VTM,-2047
 ,UTC,
 ,UZA,*BETB1+4
 ENTR/:,ITA,14
 1,AEX,BENTR-PER
 ,UZA,*BETB10
 14,XTA,1
            6,  AAX  ,CONR15
            6,  A-X  ,NKKOS
 14,UTM,2
 ,U1A,ENTR/
 14,XTA,-1
            6,  ARX  ,KOSCNT
 14,ATX,-1
             ,  UJ   ,ENTR/
 *BETB10:6,XTA,CON7
 6,ATX,CON7
 12,AEX,3
 ,UJ,*BETB1+2
 *BETB:6,XTS,RR2
 12,AEX,1
 12,STX,1
 6,AEX,CON6
 6,ARX,NGZB
 6,AAX,RCON1
 6,ARX,RR2
      6,ARX,RCON1
      6,ATX,RR2
      ,UJ,*BETB1
 *BETA:6,AEX,CON7
 ,U1A,*BETB
 12,XTA,1
            6,  AAX  ,CN00007
            6,  AEX  ,CN00001
             ,  U1A  ,*BETB1
           12,  XTA  ,1
            6,  AAX  ,CONR15
            6,  A-X  ,NKKOS
             ,  U1A  ,*BETB1
           12,  XTA  ,1
            6,  ARX  ,KOSCNT
           12,  ATX  ,1
 *BETB1:3,VZM,*+4
 3,UTM,1
 :12,XTA,3
 6,AEX,CON7
 :6,AAX,CON7
 12,UTM,2
 ,U1A,*BETA
 3,VLM,*BETB1+1
 :6,XTA,FLAGTABL
 6,AAX,CONR24
 6,AEX,CON61
 6,AEX,RCON1
 6,ATX,FLAGTABL
 13,VJM,INITLIB
 :,XTA,
 6,ATX,NCARD
 6,ATX,STN1
 6,ATX,SIGN
            6,  ATX  ,NK
            6,  ATX  ,NKCARD
            6,  ATX  ,SETCNT
            6,  ATX  ,BSSCNT
 6,ATX,BASEMOD
 6,ATX,BASLONG
 6,ATX,BASYSL
 6,ATX,SOS
 13,VTM,ABP1
 6,XTA,FLGEX*
 ,UZA,MADNAME
 ,UJ,ENDPRO1
C
C**************************************
C*
C*  ФOPMИPOBAHИE CЛOBA ИЗ ПAPЫ KOMAHД.
C*
C
 COMMAND:6,XTS,FLAGTABL
 6,AEX,CN0001
 6,ATX,FLAGTABL
 6,AAX,CN0001
 ,UZA,*COMAN1
 15,XTA,
 ,UZA,*+1
             ,  ASN  ,64-24
 :6,ATX,LEFINST
 13,UJ,
 *COMAN1:15,XTA,
 6,AEX,LEFINST
 :6,ATX,LEFINST
            6,  XTA  ,NK
            6,  ARX  ,RCON1               NK=NK+1
            6,  ATX  ,NK                  OГPAHИЧEHИE HA ДЛИHY
 PLG23:6,AEX,NKMAX
 13,U1A,
 ,UJ,FATNAME+2
 ,UTC,
 *MACB1:6,XTA,MESADR4
 ,UJ,*-4
C
C
C         ЗAПИCЬ  KOHCTAHT  LITERAL  B  TEЛO  ПOДПPOГPAMMЫ
C
C
 LITWRITE:6,XTA,KOSCNT
 13,UZA,
 ,ITA,13
 ,ITS,7
 ,ITS,3
 7,VTM,SECTOR-2
 6,XTS,CON7
 3,VTM,-2047
 6,ATX,CON7
 ,UJ,*COMANA+2
 *COMAN3:6,AEX,CON7
 ,UZA,*COMANA+1
 7,XTS,
           13,  VJM  ,CONSTPRI            ПEЧATЬ LITERAL
            7,  XTA  ,
 13,VJM,WTOLIB
            6,  XTA  ,NK
            6,  ARX  ,RCON1               NK=NK+1
 6,STX,NK
      6,AEX,CON6
      ,U1A,*COMANA
      7,XTA,1
      ,ASN,64+24
 ,UTC,
 ,ATI,13
 13,XTA,
 15,ATX,
 13,VJM,CONSTPRI
 :6,XTA,NK
 6,ARX,RCON1
 6,STX,NK
 13,VJM,WTOLIB
 *COMANA:6,XTA,CON7
 6,ATX,CON7
 :3,VZM,*+4
 3,UTM,1
 :7,XTA,3
 6,AEX,CON7
 :6,AAX,CON7
 7,UTM,2
 ,U1A,*COMAN3
 3,VLM,*-2
 :15,XTA,
 ,STI,3
 ,STI,7
             ,  ATI  ,13
           13,  UJ   ,
 EDITTT:4,UTM,-1
 ,UJ,*READ3
C
C
C         OKOHЧAHИE  TPAHCЛЯЦИИ
C
C
 ENDMADLE:,XTA,
 1,WTC,BENTR-PER
 ,ATX,
 6,ATX,2
 6,ATX,3
 14,VTM,-8
 :14,UTC,8
 6,ATX,INDTABL+1
 14,VLM,*-1
 6,XTA,C4
 6,ATX,INDTABL+4
 3,VTM,SECTOR-2
 2,VTM,-2047
 6,XTA,DEBUG
 6,ATX,DEBUGF
 6,XTA,CON7
 6,ATX,CON7
 ,UJ,*ENDMA1+1
 *DEBUG:6,AAX,CN00006
 ,UZA,*+1
 3,XTA,1
 6,AAX,CONDG
 ,U1A,*ENDMA1
 3,XTA,1
 6,AAX,C13S15
 ,U1A,*DEBUG1
 3,XTA,1
 6,AAX,CONR15
 :6,AEX,CN0004
 13,VJM,SSILKD
 :3,XTA,
 6,AAX,CONR24
 ,UZA,*+5
 6,XTA,CONR15
 6,ARX,INDTABL+4
 6,AAX,CONR15
 6,ATX,INDTABL+4
 ,ITA,3
 ,ASN,64-24
 6,AEX,CN0002
 6,ATX,ILAST
 ,UJ,*ENDMA1-1
 :3,XTA,
 ,UJ,*ENDMA1-1
 *DEBUG1:6,AEX,CN00001
 ,U1A,*DEBUG2
 3,XTA,1
 6,AAX,CONR15
 6,AEX,CN00001
 ,UJ,*DEBUG+5
 *DEBUG2:6,AEX,CN000002
 ,U1A,*DEBUG3
 3,XTA,1
 6,AAX,CONR15
 6,AEX,TOP
 ,UTC,
 12,VTM,TABDEC
 6,WTC,C4
 14,VTM,
 14,ATX,
 :12,XTA,1
 14,AEX,
 6,AAX,CONR24
 12,UTM,1
 ,U1A,*-2
 13,VJM,SSILKD+4
 :6,AEX,CNR304S9
 ,UJ,*DEBUG+5
 *DEBUG3:3,XTS,1
 ,ASN,64+24
 ,STI,12
 12,UTM,TABDEC-4000B
 6,AEX,C13S15
 13,VTM,*DEBUG3-1
 ,U1A,*DEBUG4
 6,XTA,CONR15
 3,AAX,1
 13,VTM,*+3
 ,U1A,*+4
 12,XTA,
 6,AAX,DEBMASK
 ,UZA,*DEBUG+6
 :12,UTM,-TABDEC
 ,ITA,12
 6,AEX,CN000001
 ,UJ,*DEBUG+5
 :3,XTS,1
 6,AAX,CONISK
 15,AEX,
 ,UJ,SSILKD
 *DEBUG4:6,XTA,CONR15
 3,AAX,1
 ,U1A,*-3
 ,ITA,12
 6,AEX,CN000003
 ,UJ,*DEBUG+5
 *ENDMA3:6,XTA,C4
 1,ARX,ENMCON-PER
 6,AAX,CONR15
 ,ASN,64-24
 6,ATX,RR1
 2,XTA,
 15,ATX,
 ,UJ,*+7
 :2,XTA,
 15,ATX,
 6,AAX,CN0002
 ,UZA,*+5
 6,XTA,ILAST
 6,ARX,CONDLI1
 6,ATX,ILAST
 6,ARX,RR1
 15,AEX,-1
 6,AAX,MASK24
 15,AEX,-1
 15,ATX,-1
 :15,XTA,-1
 13,VJM,TOPRINT
 :15,XTA,
 13,VJM,WTOLIB
  :2,UTM,1
 ,ITA,2
 6,AEX,C4
 ,U1A,*-9
 6,XTA,ILAST
 ,ASN,64+24
 6,ATX,ILAST
 3,VTM,TABDEC
 2,VTM,
 ,ATI,4
 :3,XTA,1
 6,AAX,CN0002
 3,UTM,1
 ,UZA,*-1
 3,XTA,
 ,ASN,64+24
 4,UTM,-1
 ,ATI,5
 5,XTA,
 13,VJM,TOPRINT
 :5,XTA,
 13,VTM,*-5
 4,V1M,WTOLIB
 13,VJM,WTOLIB
 6,XTA,ILAST
 ,UJ,*ENDMA5
 :3,XTA,
 ,UJ,*ENDMA1-3
 *ENDMA:6,XTA,RCON1
 6,ARX,INDTABL+3
 6,ATX,INDTABL+3
 3,XTA,1
 PLGD:6,AAX,CN00006
 ,UZA,*ENDMA1
 3,XTA,1
 6,AAX,CN00007
 6,AEX,CN00003
 6,ARX,NGZB
 6,AAX,RCON1
 15,ATX,
 6,ARX,INDTABL+1
 6,STX,INDTABL+1
 6,AEX,RCON1
 6,ARX,INDTABL+2
 6,ATX,INDTABL+2
 3,XTA,
 :6,AAX,CONR24
  ,UZA,*ENDMA-1
 :,ITA,3
 ,ASN,64-24
 6,AEX,CN0002
 6,ATX,ILAST
 :3,XTS,1
 ,ASN,64+24
 ,STI,12
 12,UTM,TABDEC-4000B
 :12,AEX,
 12,ATX,
 *ENDMA1:2,VZM,*+3
 2,UTM,1
 :3,XTA,3
 6,AAX,CON7
 3,UTM,2
 ,UZA,*ENDMA
 :2,VLM,*-2
 ,XTA,ENTRY*
 2,VTM,TABDEC
 2,ATX,
 6,XTA,LIS*G
 6,AOX,MAINPRIN
 6,ATX,MAINPRIN
 6,XTA,ILAST
 ,ITS,
 6,ATX,OCTAL
 6,STX,ILAST
 ,U1A,*ENDMA3
 :2,XTA,
 13,VJM,TOPRINT
 :2,XTA,
 13,VJM,WTOLIB
 :2,UTM,1
 ,ITA,2
 6,AEX,C4
 ,U1A,*-3
 *ENDMA5:6,ATX,4
 6,XTA,SETCNT
 ,NTR,3
 6,ATX,9
 6,X-A,NK
 6,A-X,NKDATA
 6,ATX,8
 6,XTA,NKABS
 6,ATX,5
 6,X-A,NKDATA
 6,ATX,7
 6,XTA,C4
 1,A-X,ATC-PER
 6,ATX,1
 1,XTA,TABADR-PER
 1,X-A,ATC-PER
 6,ATX,
 1,XTA,BENTR-PER
 1,A-X,ENTRADR-PER
 ,ASN,64+1
 6,ATX,INDTABL
 6,XTA,C4
 6,A-X,INDTABL+4
 6,ATX,INDTABL+4
 6,XTA,GLOB*
 6,ATX,INDTABL+7
 6,XTA,LIS*G
 6,AEX,MAINPRIN
 6,ATX,MAINPRIN
 6,XTA,BSSCNT
 6,ATX,6
 6,XTA,NKCARD
 6,ATX,INDTABL+6
 6,XTA,DDT
 6,ATX,INDTABL+8
 6,XTA,MODTEXT
 6,ATX,INDTABL+9
 ,XTA,ENTRY*
 6,AEX,STEXTPNT
 13,VTM,*RETURN*+1
 1,VTM,PROCTAB
 ,UZA,GSTEXT*
 13,VJM,TOPRINT1
 13,VJM,STOLIB
 6,XTA,STEXTPNT
 ,UZA,*+2
 1,VTM,PROCTAB
 ,CALL,GSTEXT*
 :12,VTM,
 14,VJM,MDL*REF
 *ENDMA90:13,Z31,TIME
 ,UJ,PURGE1
 13,VJM,EJECT
 PURPLC:13,Z31,PRINT8**+1
 ,UJ,PURGEC
 :6,XTA,FLGEX*
 ,ASN,64-2
 ,AOX,ERROFLG*
 ,ATX,ERROFLG*
 *RETURN*:6,WTC,OUTMADLE
 ,UJ,
 :13,VTM,*ENDMA90-1
 ,UJ,TOPRINT1
 ERSIM:6,XTA,RCON1
            6,  ATX  ,IMIST
             ,  UJ   ,ABP
 ERSIM1:6,XTS,RCON1
 6,STX,IMIST
 ,UJ,WRITE
 ERUTC:6,ATX,MESKOP
 13,VJM,*WRI
 6,ATX,MISTABL
 6,XTA,IFPLG
 ,U1A,*APKOP+1
 6,XTA,PSKPLG
 ,U1A,POSKOP-1
 ,UJ,*ABP1A
C
C**************************************
C*
C*  ФATAЛЬHAЯ ДИAГHOCTИKA.
C*
C
 FATNAME:13,VTM,*FNAME
             ,  UJ   ,FATERR
           13,  VTM  ,*FNAME
             ,  UJ   ,FATERR+1
           13,  VTM  ,*FNAME
             ,  UJ   ,FATERR+2
 :13,VTM,FATERRD-2
 ,UJ,SKIPREE
           13,  VTM  ,*FNAME
             ,  UJ   ,FATERR+4
 *FNAME:14,VTM,FATNAME+3
 ,ITA,14
 6,ATX,IFPLG
 13,VJM,GERPRI
 :6,XTA,MESKOP
 ,UTC,END*T
 ,AEX,
 ,UZA,FATERRD
 13,VJM,FATERR+3
 :,XTA,
 6,ATX,AB*PLG
 6,ATX,MESREAD*
 13,VJM,SKIPREE
 :6,XTA,MESKOP
 ,UTC,END*T
 ,AEX,
 ,U1A,FATERRD+2
 FATERRD:6,XTA,CNR10000
 6,AOX,FLGEX*
 6,ATX,FLGEX*
  ,UJ,*ENDMA90
 :13,VTM,*ABP1
 ,UJ,*READ4
C
C+++++++++++++++++++++++++++++++++++++++
C+
C+   ПEPEHOC ПOMEЧEHHOCTИ ПO TPEБOBAHИЮ
C+   ПPEДЫДYЩEЙ KOMAHДЫ.
C+
C
 :,XTA,
 6,ATX,ABSCNT
 6,ATX,BSSCNT
 ,NTR,7
 FLAG2T:6,XTA,CN0002
 6,ATX,STN1
 6,AAX,FLAGTABL
 ,ASN,64-1
 6,XTS,FLAGTABL
 6,AAX,CN0004
 ,ASN,64+1
 6,XTS,FLAGTABL
 6,AAX,NCN0006
 15,AEX,
 6,STX,FLAGTABL
 6,AOX,MISTABL
 6,AAX,CN0004
 13,UZA,
 :6,ATX,STN1
 ,ASN,64+2
 :6,AAX,FLAGTABL
 ,UZA,MET
 6,AEX,FLAGTABL
 6,ATX,FLAGTABL
 ,ITA,13
 1,XTS,UJ13-PER
 :6,XTS,UTCCONST
 6,AEX,LEFINST
 PLG*LEF:13,VTM,*FLAG2T
 ,UJ,*COMAN1+1
 :15,XTA,
 13,VJM,*COMAN1+1
 :6,XTA,NK
 6,AEX,NKKOS
 13,VTM,*+1
 ,UZA,LITWRITE
 *FLAG2T:15,XTA,
             ,  ATI  ,13
 UJ13:13,UJ,
 ,UJ,MET-1
 :6,XTA,CN0004
 ,UJ,PLG*LEF-5
 LEFCOM:6,XTA,CN0001
 6,ATX,STN1
 6,AAX,FLAGTABL
 13,UZA,
 6,AEX,FLAGTABL
 6,ATX,FLAGTABL
 ,ITA,13
 ,UJ,PLG*LEF-1
C
C+++++++++++++++++++++++++++++++++++++++
C+
C+   KOHCTPYKЦИЯ: AДPEC = ИДEHTИФИKATOPY
C+
C
 IDADR:,ITA,13
 ,ITS,12
 1,XTS,7
 13,VJM,*RENA+1
 :6,XTA,POSIND
 13,VTM,LOI
 12,VTM,*+2
 ,U1A,*WRIN
 6,XTS,RCON1
 6,STX,IMIST
         :  6,  STX  ,MESADR1
 ,STI,12
             ,  ATI  ,13
 6,XTA,IMIST
 13,UZA,
 6,XTA,CON02
            6,  AOX  ,MISTABL
            6,  ATX  ,MISTABL             OШИБKA B AДPECE
 ,XTA,
            6,  ATX  ,IMIST
           13,  UJ   ,
C
C
C         OБPAБOTKA  ИДEHTИФИKATOPOB-BXOДOB
C
C
 :6,XTA,MISTABL
 6,AOX,CON2
 6,ATX,MISTABL
 1,XTA,CADAVER-PER
 ISKENTRY:,UZA,*-2
 6,STX,RR1
 6,AOX,CON2
 6,ATX,RR2
 12,VTM,ENTRY*
 14,VTM,-ENTRY*-40
 *ISKENT1:,ITA,12
 1,AEX,BENTR-PER
             ,  UZA  ,*ISKENTA
            6,  XTA  ,RR1
           12,  AEX  ,
 12,UTM,2
 ,U1A,*ISKENT1
 12,XTA,-1
 PLG2:6,AOX,CON02
 12,ATX,-1
 13,UJ,
 :12,XTA,-2
             ,  UJ   ,TWODEC
 *ISKENTA:  6,  XTA  ,RR1                 ЗAHECEHИE BXOДA B TAБЛИЦY
 12,J+M,14
 14,VZM,FATNAME+1
           12,  ATX  ,
            6,  XTA  ,RR2
           12,  ATX  ,1
           12,  UTM  ,2
             ,  ITA  ,12
 1,ATX,BENTR-PER
 13,UJ,
 CADAVER:,TEXT,8H CADAVER
C
C**************************************
C*
C*  ПOИCK ИДEHTИФИKATOPA B TAБЛИЦE
C*  ИДEHTИФИKATOPOB, ПOЛYЧEHИE COOTBET-
C*  CTBYЮЩEГO AДPECA.
C*
C
 ISKADR:6,ATX,RR1
 ,XTA,
 6,ATX,RR2
 14,VJM,POISKI
 PLG3:14,VTM,*IS*1
 ,U1A,*ISKADR
 CROSS:6,XTA,LISLAST
 6,AAX,LIS*R
 14,UZA,
 6,XTA,T*ЬЬЬЬ
 12,AEX,
 6,AAX,AA6SIMB
 :,U1A,MDL*REF
 14,UJ,
 *IS*1:12,XTA,1
 6,AOX,CON04
 12,ATX,1
 6,AAX,CON004
 ,UTC,
 ,U1A,NO*DEF
 *IS*2:12,XTA,1
 6,AAX,CN00007
 ,UZA,*IS*2Q
 6,AEX,CN00001
 ,U1A,*IS*2Q+5
 12,XTA,1
 6,AAX,CN000002
 6,ARX,NCON4
 6,AAX,CON4
 6,AEX,CON1
 :12,XTS,1
 ,UTC,
 :6,AAX,CONR15
 13,UJ,
 *IS*2Q:12,XTA,1
 6,AAX,CN000003
 ,UZA,*IS*2Q-2
 ,ASN,64+17
 ,YTA,
 12,XTS,1
 ,ASN,64+24
 6,AAX,CONR15
 15,AEX,
 ,UJ,*IS*2Q-2
 :12,XTA,1
 ,ASN,64+24
 6,AAX,CONR15
 6,AEX,CON2
 ,ITS,
 13,UJ,
 *IS*3:12,XTA,1
 6,AAX,CON045
 ,UZA,*IS*2
 6,AEX,CON041
 ,UZA,*IS*2+5
 6,AEX,CON005
 ,UZA,*+7
 6,XTA,CON04
 6,ATX,RR2
 ,ASN,64+2
 12,AAX,1
 ,ASN,64-3
 6,AEX,RR2
 ,ASN,64-4
 ,YTA,
 6,AOX,BLDEF
 6,ATX,BLDEF
 12,XTA,1
 6,AAX,CON042
 6,AEX,CON042
 :,U1A,*IS*2
 ,ITA,13
 6,XTS,RR1
 13,VJM,NODEC
 :15,WTC,
 13,VTM,
 NO*DEF:6,XTA,CON1
 6,XTS,CON04
 6,ATX,RR2
 ,ASN,64+2
 12,AAX,1
 ,ASN,64-4
 15,AEX,-1
 ,ASN,64-3
 ,YTA,
 6,AOX,BLDEF
 6,ATX,BLDEF
 ,ITA,
 12,VTM,
 13,UJ,
 *ISKADR:,ASN,64+1
 14,VTM,NO*DEF
 ,UZA,FATNAME+1
 6,XTA,RR1
 12,ATX,
 6,XTA,NO*DEC
 12,ATX,1
 ,UJ,CROSS
C
C**************************************
C*
C*   OБPAБOTKA METKИ.
C*
C
 :15,WTC,
 13,VTM,
 MET:6,XTA,MESMET
 13,UZA,
 6,XTA,MISTABL
 6,AAX,CON2
 13,U1A,
 6,XTA,NK
 6,AEX,CN00001
 6,AEX,SYSREL
 6,XTS,FLAGTABL
 6,AAX,CON004
 ,ASN,64+20
 15,AEX,
 6,XTS,MESMET
C
C**************************************
C*
C*  ЗAHECEHИE ИДEHTИФИKATOPA B TAБЛИЦY
C*  ИДEHTИФИKATOPOB.
C*
C
 TOTIN:6,STX,RR1
 6,ATX,RR2
 PLG4:14,VJM,POISKI
 :13,MTJ,14
 ,UZA,*TOTIN2
 ,ASN,64+1
 ,UZA,FATNAME+1
 6,XTA,RR1
 12,ATX,
 *TOTIN0:6,XTA,RR2
 6,AAX,CN00006
 ,UZA,*TOTIN1
 6,XTA,C4
 ,ATI,14
 6,ARX,RCON1
 6,ATX,C4
 1,AEX,BBB-PER
 14,UTM,4000B-TABDEC
 ,UZA,FATNAME+1
 ,ITA,14
 ,ASN,64-24
 6,AEX,RR2
 6,AAX,CON*T
 6,ATX,RR2
 6,AAX,CONR24
 6,AOX,CN0004
 ,UTC,TABDEC-4000B
 14,ATX,
 13,MTJ,14
 *TOTIN1:6,XTA,CON04
 6,AOX,RR2
 12,ATX,1
 6,XTA,LISLAST
 6,AAX,LIS*R
 13,UZA,
 6,XTA,T*ЬЬЬЬ
 12,AEX,
 6,AAX,AA6SIMB
 ,U1A,MDL*REF+1
 13,UJ,
 *TOTIN2:12,XTA,1
 6,AEX,NO*DEC
 ,UZA,*TOTIN0
 12,XTA,1
 6,AAX,CN00007
 ,U1A,*TOTIN3
 12,XTA,1
 6,AAX,CON063
 6,AEX,CON001
 ,UZA,*TOTIN0
 6,AEX,CON043
 6,XTS,RR2
 6,AAX,CN00007
 15,AEX,
 :,UZA,*+2
 12,XTA,1
 6,AOX,CON02
 ,UJ,*+2
 :6,XTA,CON042
 6,AOX,RR2
 :12,ATX,1
 ,UJ,CROSS
 *TOTIN3:6,AEX,RR2
 6,AAX,CN00007
 12,XTS,1
 6,AOX,RR2
 6,AAX,CONSB
 6,AEX,CONSUB
 15,AOX,
 ,U1A,*TOTIN3-4
 6,XTA,RR2
 6,AAX,CON01
 13,U1A,
 12,XTA,1
 6,AAX,NCON01
 ,UJ,*TOTIN3-1
C
C**************************************
C*
C*  BTOPOЙ ПPOXOД.
C*
C
 :12,XTA,1
 6,AAX,CN00004
 13,UZA,
 ,UJ,*ISKOP4+4
 ISKOP:6,XTA,VERET
 ,U1A,*+5
 12,XTA,1
 6,AAX,CONISK
 ,U1A,*+2
 6,XTA,RR2
 6,AAX,CONISK
 ,UZA,*+2
 :,ASN,64-12
 6,ATX,VERET
 :12,XTA,1
 6,AEX,CON04
 12,ATX,1
 6,AOX,RR2
 6,AAX,CON066
 ,UZA,ISKOP-2
 6,AAX,CON02
 ,U1A,*ISKOP2
 12,XTA,1
 6,AAX,CN00007
 ,U1A,*ISKOP4
 12,XTA,1
 6,AAX,CON002
 ,UZA,*ISKOP1
 6,AOX,RR2
 12,ATX,1
 6,XTA,RDLITS
 6,ATX,RDFLAG
 :13,MTJ,14
 ,UJ,CROSS
 *ISKOP1:6,XTA,CON004
 6,AOX,RR2
 12,ATX,1
 13,UJ,
 *ISKOP2:6,AOX,RR2
 6,ATX,RR2
 6,XTA,CON041
 12,AAX,1
 ,UZA,*+2
 6,XTA,CON04
 12,AEX,1
 ,UJ,*ISKOP3
 :12,XTA,1
 6,AAX,CON002
 ,UZA,*ISKOP3
 6,AOX,RR2
 12,ATX,1
 ,XTA,
 *ISKOP3:,ITS,12
 ,ITS,13
 12,XTS,
 13,VJM,TWODEC
 :15,XTA,
 ,STI,13
 ,STI,12
 13,UZA,
 12,ATX,1
 ,UJ,*ISKOP1-1
 *ISKOP4:6,XTS,CON076
 6,AAX,RR2
 12,AOX,1
 6,AAX,NCON04
 12,STX,1
 6,AEX,CN00001
 ,UTC,
 ,UZA,*ISKOP5
 :12,XTA,1
 ,ASN,64+24
 ,ATI,14
 14,UTM,TABDEC-4000B
 14,XTA,
 6,ATX,IRE
 6,XTA,RR2
 6,AAX,CONR24
 6,AOX,CN0004
 14,ATX,
 6,AAX,CN000001
 ,ASN,64+4
 14,AEX,
 14,ATX,
 13,UJ,
 *ISKOP5:6,XTA,CON004
 12,AAX,1
 13,UZA,
 6,XTA,CON076
 12,AAX,1
 6,AOX,RR2
 12,ATX,1
 13,UJ,
C
C**************************************
C*
C*  ФOPMИPOBAHИE CTPOKИ TAБЛИЦЫ OПИCAHИЙ
C*
C
 SSILKD:6,ATX,-1
 12,VTM,TABDEC
 :6,WTC,C4
 ,ATX,
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,U1A,*-1
 :,ITA,12
 6,AEX,C4
 ,U1A,SSILKSD1+8
 6,XTA,C4
 :6,ARX,RCON1
 6,ATX,C4
 1,AEX,BBB-PER
 ,U1A,SSILKSD1+8
 :6,XTA,DEBUGF
 13,VTM,FATERRD
 ,UZA,FATNAME+1
 ,UJ,FATERR+1
 SSILKEXD:6,WTC,MESADR1
 12,VTM,TABDEC-4000B
 :6,ATX,-1
 ,UJ,SSILKD+1
 SSILKSD:6,XTA,MESADR1
 ,ASN,64-24
 6,AEX,MESADR2
 6,XTS,XADRESS1
 6,AEX,XADRESS2
 6,WTC,MESADR1
 12,VTM,TABDEC-4000B
 ,U1A,*+3
 6,ATX,XADRESS2
 6,STX,MESADR2
 6,AEX,CNR104S9
 ,UJ,SSILKRD+6
 :15,XTA,-1
 ,ASN,64-24
 6,AEX,MESADR1
 15,AEX,-1
 6,STX,-2
 6,AEX,CNR304S9
 6,ATX,-1
 ,XTA,
 6,ATX,XADRESS2
 6,ATX,MESADR2
 6,XTA,XADRESS1
 ,NTR,13B
 ,U1A,*+3
 6,XTA,-2
 SSILKSD1:6,AEX,-1
 6,ATX,-1
 :6,XTA,CON2
 6,ATX,XADRESS1
 :6,XTA,-1
 ,UTC,
 6,WTC,C4
 ,ATX,
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,UZA,SSILKD+4
 6,AEX,-2
 ,U1A,*-2
 6,XTA,CON6
 6,ATX,XADRESS1
 :12,UTM,4000B-TABDEC
 ,ITA,12
 :12,UTM,TABDEC-4000B
 13,UJ,
 SSILKRD:6,XTA,MESADR3
 6,WTC,MESADR1
 12,VTM,TABDEC-4000B
 ,UZA,ADR1*0+2
 6,ATX,-3
 6,XTA,MESADR1
 ,ASN,64-24
 6,AEX,-3
 6,XTS,XADRESS1
 6,AAX,CON4
 ,U1A,*+2
 6,STX,MESADR3
 :6,ATX,-1
 ,UJ,SSILKD+1
 :6,XTA,CONR15
 15,ARX,-1
 6,AEX,CONR16
 6,STX,-1
 6,AAX,RCONY
 ,U1A,*+5
 6,ATX,MESADR3
 6,XTA,-3
 ,ASN,64-24
 6,AEX,MESADR1
 6,AEX,CNR304S9
 6,AEX,-1
 6,ATX,-2
 ,UJ,SSILKSD1
 :,XTA,
 6,ATX,MESADR3
 6,XTA,MESADR1
 6,AEX,CONNG
 6,WTC,C4
 ,ATX,
 6,AEX,-1
 6,ATX,-2
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,UZA,SSILKSD1+8
 6,AEX,-2
 6,AAX,CONRV
 ,U1A,*-3
 ,ITA,12
 6,AEX,C4
 ,UZA,SSILKRD1
 12,XTA,
 ,ASN,64+24
 6,ARX,CONR15
 6,AEX,CONR15
 6,ARX,-3
 6,AAX,CONR15
 12,UTM,4000B-TABDEC
 ,ITS,12
 ,ASN,64-24
 12,UTM,TABDEC-4000B
 15,AEX,
 6,AEX,-1
 6,ATX,-2
 ,UJ,SSILKSD1
 SSILKRD1:6,XTA,CON2
 6,ATX,XADRESS1
 :12,UTM,4000B-TABDEC
 ,ITA,12
 ,ASN,64-24
 6,AEX,-3
 12,UTM,TABDEC-3776B
 12,ATX,-1
 ,ITA,12
 6,ATX,C4
 12,UTM,3777B-TABDEC
 ,ITA,12
 6,AAX,CNR4000
 ,UZA,SSILKD+8
 :,ITA,12
 ,UJ,SSILKSD1+9
 SSILKRI:6,XTA,MESADR3
 ,XTS,
 6,STX,MESADR3
 6,ATX,MESADR1
 6,AEX,XADRESS1
 6,ATX,MESADR4
 ,NTR,13B
 ,UZA,SSILKR+7
 6,XTA,MISTABL
 6,AAX,CN0002
 ,U1A,SSILKR+6
 6,XTA,CON2
 6,ATX,XADRESS1
 6,XTA,MESADR1
 SSILKR:6,WTC,YSLNYL
 12,VTM,
 6,AEX,TOP
 6,ATX,-1
 :12,V1M,SSILKD+1
 6,WTC,C4
 12,VTM,-1
 12,ATX,1
 ,ITA,12
 6,ATX,YSLNYL
 6,XTA,C4
 12,VLM,SSILKD+6
 :6,XTA,MESADR1
 13,UJ,
 :6,XTA,MISTABL
 6,AAX,CN0002
 ,U1A,MINUS-1
 6,XTA,CON2
 6,ATX,XADRESS1
 6,XTA,MESADR1
 SSILKNR:6,WTC,GLOKOP
 12,VTM,
 12,VZM,*+4
 6,ARX,NGRSS
 6,AAX,M47S15
 6,ATX,-1
 6,AAX,CONR15
 ,UZA,SSILKSD1+8
 6,XTA,-1
 ,UJ,SSILKD+1
 :6,ARX,CONR15
 6,AEX,CONR15
 6,AAX,CONR15
 6,ATX,NGRSS
 ,ITA,13
 6,ATX,-2
 6,XTA,NGRSS
 13,VJM,SSILKR
 :6,AEX,CONNG
 13,VJM,SSILKRD+6
 :6,WTC,-2
 13,VTM,
 6,ATX,-1
 ,ITS,12
 6,STX,GLOKOP
 ,ASN,64-24
 6,AEX,NGRSS
 6,ATX,NGRSS
 6,XTA,-1
 13,UJ,
 :6,XTA,MESADR1
 ,UTC,
 MINUS:6,ARX,CONR15
 6,AEX,CONR15
 6,AAX,CONR15
 13,UJ,
 SSILKRR:6,WTC,YSLNYL
 12,VTM,
 6,STX,-1
 ,ASN,64+7
 15,UTM,1
 ,NTR,7
 6,XTS,-1
 15,AVX,
 6,AAX,CONR15
 6,AEX,TOP
 12,VZM,SSILKR+2
 6,WTC,C4
 ,ATX,
 6,ATX,-1
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,U1A,*-1
 ,ITA,12
 6,AEX,C4
 ,U1A,SSILKSD1+8
 6,WTC,YSLNYL
 12,VTM,1
 6,XTA,-1
 12,A-X,
 15,UTM,1
 15,AVX,
 6,AAX,CONR15
 6,ATX,MESADR3
 ,UJ,SSILKSD1+8
 SSILKC:6,WTC,CSSILK
 12,VTM,
 6,AEX,CN0004
 6,ATX,-1
 :12,V1M,SSILKD+1
 6,WTC,C4
 12,VTM,-1
 12,ATX,1
 ,ITA,12
 6,ATX,CSSILK
 6,XTA,C4
 12,VLM,SSILKD+6
 SSILKCC:6,WTC,CSSILK
 12,VTM,
 6,AEX,CN0004
 12,VZM,SSILKC+2
 6,WTC,C4
 ,ATX,
 6,ATX,-1
 6,XTA,CNR7S12
 6,ATX,-2
  14,VTM,SSILKD+4
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 14,UZA,
 6,AEX,-2
 ,U1A,*-2
 :6,XTA,CN00001
 6,AEX,MESKOP
 6,ATX,MESKOP
 ,UJ,SSILKSD1+8
 SSILKC2:6,WTC,CSSILK
 12,VTM,
 6,XTA,MESADR1
 14,VTM,SSILKD+4
 :6,AEX,CN0004
 6,ATX,-1
 6,XTA,MESADR1
 6,AEX,MESADR3
 6,ATX,-2
 6,XTA,MESADR1
 6,AEX,NGZB
 6,ARX,MESADR3
 ,U1A,*+3
 ,XTA,
 :6,ATX,MESMOD
 6,XTA,-2
 6,AEX,-1
 6,ATX,-1
 :6,XTA,-1
 12,VZM,SSILKC+2
 6,WTC,C4
 ,ATX,
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 14,UZA,
 6,AEX,-2
 ,U1A,*-2
 6,XTA,BASEMOD
 6,AEX,MESMOD
 6,ATX,MESMOD
 ,UJ,SSILKSD1+8
 SSILKC4:6,WTC,CSSILK
 12,VTM,
 6,XTA,MESKOP
 12,VZM,*+10
 6,ATX,-3
 6,AEX,CN00001
 6,ATX,MESKOP
 6,XTA,MESADR1
 6,AEX,CNR7S12
 14,VJM,SSILKC2+2
 :,ITA,12
 6,AEX,C4
 ,U1A,SSILKSD1+8
 6,XTA,-3
 6,ATX,MESKOP
 6,XTA,-1
 6,AEX,CNR7S12
 6,ATX,-1
 14,VTM,SSILKD+4
 6,XTA,BASEMOD
 6,AEX,MESMOD
 ,UJ,SSILKC2+7
 :6,XTA,MESADR1
 6,AEX,NGZB
 6,ARX,MESADR3
 ,U1A,*+2
 6,XTA,MESADR1
 ,UJ,SSILKC+1
 :,XTA,
 6,ATX,MESMOD
 6,XTA,MESADR3
 ,UJ,SSILKC+1
 SSILKC31:6,WTC,CSSILK
 12,VTM,
 6,XTA,MESADR1
 6,AEX,CN0004
 12,VZM,SSILKC+2
 6,WTC,C4
 ,ATX,
 6,ATX,-1
 6,XTA,CNR7S12
 6,ATX,-2
 6,AEX,MESADR3
 6,ATX,-3
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,UZA,SSILKD+4
 6,AEX,-2
 ,UZA,SSILKCC+8
 6,AEX,-3
 ,U1A,*-3
 :6,ATX,MESMOD
 ,UJ,SSILKSD1+8
 SSILKC33:6,WTC,CSSILK
 12,VTM,
 ,XTA,
 6,ATX,MESMOD
 6,XTA,MESADR3
 6,AEX,CN0004
 12,VZM,SSILKC+2
 6,WTC,C4
 ,ATX,
 6,ATX,-1
 6,XTA,CNR7S12
 6,ATX,-2
 6,AEX,MESADR1
 6,ATX,-3
 :12,XTA,1
 6,AEX,-1
 12,UTM,1
 ,UZA,SSILKD+4
 6,AEX,-2
 ,UZA,SSILKCC+8
 6,AEX,-3
 ,U1A,*-3
 6,XTA,BASEMOD
 ,UJ,SSILKC33-1
 SSILKCL:6,WTC,CSSILK
 12,VTM,
 6,XTA,MESADR3
 12,VZM,SSILKC+1
 6,AEX,CONCL
 6,ATX,-1
 6,XTA,MESADR3
 6,AEX,CN0004
 6,WTC,C4
 ,ATX,
 :12,XTA,1
 6,ARX,-1
 12,UTM,1
 ,UZA,*-1
 6,AEX,CONR15
 6,ATX,-2
 6,AAX,RCONY
 ,UZA,*+4
 6,AEX,CNR7S12
 ,U1A,*-4
 :6,XTA,-2
 6,AAX,CONR12
 6,ATX,MESADR2
 ,UJ,SSILKCC+8
 :6,XTA,-2
 6,AAX,CONR12
 6,ATX,MESADR2
 ,UJ,SSILKD+4
 SSILKCR:6,XTA,MESADR3
 13,VTM,*ETAP3RL+6
 6,ARX,XADRESS1
 ,UZA,*+12
 6,WTC,YSLNYL
 12,VTM,
 6,AEX,TOP1
 12,VZM,SSILKR+2
 :6,WTC,C4
 ,ATX,
 6,AEX,CONR15
 6,ATX,-1
 :12,XTA,1
 6,AEX,-1
 6,AAX,MASK33
 12,UTM,1
 ,U1A,*-2
 12,XTA,
 6,ARX,-1
 6,AEX,CONR15
 6,ATX,-2
 6,AAX,RCONY
 ,UZA,SSILKCL+12
 1,AEX,-3
 ,U1A,*-6
 ,UJ,SSILKCL+10
 :6,WTC,GLOKOP
 12,VTM,
 12,VZM,SSILKNR+5
 6,ARX,NGRSS
 6,AAX,CONR15
 6,ATX,MESADR3
 12,UTM,4000B-TABDEC
 ,ITS,12
 6,STX,MESADR1
 ,UJ,SSILKCE+2
 SSILKCE:6,XTA,MESADR3
 13,VTM,*ETAP3RL+6
 6,ARX,XADRESS1
 ,UZA,*+8
 :6,WTC,MESADR1
 12,VTM,TABDEC-4000B
 :6,AAX,RMASK11
 6,AEX,MESADR3
 ,UZA,*+3
 6,XTA,MESADR1
 ,ASN,64-24
 6,AEX,MESADR3
 ,UJ,SSILKCR+4
 :6,XTA,MESADR3
 6,ATX,MESADR2
 6,XTA,MESADR1
 13,UJ,
 :6,WTC,MESADR1
 12,VTM,TABDEC-4000B
 6,ATX,-2
 6,XTA,MESADR1
 6,AEX,CONNG
 6,WTC,C4
 ,ATX,
 6,ATX,-1
 12,XTA,1
 6,AEX,-1
 6,AAX,CONRV
 12,UTM,1
 ,U1A,*-2
 ,ITA,12
 6,AEX,C4
 ,UZA,*+7
 12,XTA,
 ,ASN,64+24
 6,ARX,CONR15
 6,AEX,CONR15
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 12,UTM,4000B-TABDEC
 ,ITS,12
 6,STX,MESADR1
 12,UTM,TABDEC-4000B
 ,UJ,SSILKCE+3
 :6,XTA,-2
 6,AAX,RCONY
 ,UZA,SSILKCL+12
 6,AEX,-3
 ,U1A,*+3
 6,XTA,CN00001
 6,AEX,MESKOP
 6,ATX,MESKOP
 ,UJ,SSILKCL+12
 :6,XTA,MESADR3
 ,UZA,*+6
 6,ARX,MASK37
 6,ATX,-2
 6,XTA,RMASK11
 ,ASN,64-24
 6,AEX,CN0004
 12,AEX,
 12,ATX,
 ,XTA,
 6,ATX,MESADR3
 ,UJ,*-10
 :6,XTA,-2
 6,ATX,-3
 ,UJ,SSILKRD1+1
C
C**************************************
C*
C*  ПEPEBOД CИMBOЛA ISO B KOД GOST.
C*
C
 ISOGOST:6,ATX,R1
 ,NTR,7
 6,A*X,CON/6
 15,ATX,
 ,ASN,64-1
 15,ATX,
 ,ASN,64-1
 15,ARX,
 6,ARX,RCON5
 6,A-X,R1
 ,NTR,6
 ,ASN,64-3
 15,WTC,
 6,XTS,I
 15,WTC,
 ,ASN,64
 6,AAX,RCON377
 15,ATX,
 6,ARX,CONG
 6,AAX,CON4
 15,ARX,
 13,U1A,
 ERRISIS:6,XTS,MISTABL
 6,AOX,CON02
 6,STX,MISTABL
 13,UJ,
C
C**************************************
C*
C*  ПEPEBOД CИMBOЛA ISO B KOД TEXT.
C*
C
 ISOTEXT:6,ARX,MB40
 6,AAX,CONR15
 13,UZA,
 15,ATX,
 6,AAX,CNR40000
 ,ASN,64-1
 6,ARX,NGZB
 15,AAX,-1
 15,ATX,-1
 ,ASN,64+3
 6,STX,R1
 6,AEX,RCON7
 6,AAX,RCON7
 ,ASN,64-1
 15,ATX,
 ,ASN,64-1
 15,ARX,
 6,WTC,R1
 6,XTS,I1
 15,WTC,
 ,ASN,64
 6,AAX,AA6SIMBR
 13,U1A,
 ,UJ,ERRISIS
C
C**************************************
C*
C*  ПEPEBOД CИMBOЛA ISO B KOД TEL.
C*
C
 ISOTLG:,ATI,14
 6,XTA,RCON377
 14,UTM,-76B
 14,V1M,ISOTL
 6,ATX,RE*RTLG
 ,ITA,13
 6,ATX,R1
 13,VJM,READ
 :,XTA,
 14,UTM,-76B
 6,WTC,R1
 13,VTM,
 ISOTL:6,ATX,Q*2RENS
 14,UTM,76B-40B
 ,ITA,14
 6,AAX,CNR40000
 ,U1A,TLERR
 ,ITA,14
 ,NTR,7
 6,A*X,CON/6
 15,ATX,
 ,ASN,64-1
 15,ATX,
 ,ASN,64-1
 15,ARX,
 6,ARX,RCON5
 ,ITS,14
 15,X-A,
 ,NTR,6
 ,ASN,64-3
 15,WTC,
 6,XTS,I2
 15,WTC,
 ,ASN,64
 6,AAX,RCON377
 6,ATX,R1
 6,AAX,Q*2RENS
 13,U1A,
 6,XTA,R1
 14,VTM,-2
 TLREGS:,EQU,STROKA+TLREG
 :14,UTM,TLREGS+2
 14,AEX,
 ,UZA,ISOTL1
 6,XTA,R1
 14,UTM,-TLREGS-2
 14,VLM,*-2
 6,AEX,TLZ
 ,U1A,TLERR
 6,ATX,RE*RTLG
 ,UJ,*REISA1
 ISOTL1:14,XTA,TLREGC-TLREG
 13,UJ,
 TLERR:6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 ,UJ,*REISK-1
C
C**************************************
C*
C*  ЗAHECEHИE KOHCTAHT LITERAL B
C*  TAБЛИЦY ИДEHTИФИKATOPOB.
C*
C
 ISKKOS:6,STX,RR1
            6,  ATX  ,RR2
 PLG5:14,VTM,ISKK2+1
 ,UJ,POISKL
 :6,AOX,BLDEF
 6,ATX,BLDEF
 14,VTM,ISKK2
 6,XTA,RR2
 6,AEX,CON7
 ,U1A,POISKL
 14,VJM,POISKL
 ,ITA,12
 ,ASN,64-24
 6,AEX,CON6
 6,STX,RR2
 6,ATX,RR1
 14,VJM,POISKL
 ISKK2:6,XTA,CON1
 ,UJ,*IS*2+3
 :,UZA,*ISKK1A
            6,  AEX  ,RCON1
             ,  UZA  ,FATNAME+1           ПEPEПOЛHEHИE TИ
 6,XTA,RR1
           12,  ATX  ,
            6,  XTA  ,RR2
           12,  ATX  ,1
      6,AAX,CON7
      6,AEX,CON7
      ,UZA,*ISKK1B.BTOPAЯ ЧACTЬ DOUBLE
      6,AEX,CON1
 6,ARX,NGZB
 6,AAX,RCON1
 6,ARX,RCON1
 6,ARX,KOSCNT
      6,ATX,KOSCNT
 :6,XTA,CON1
 6,XTS,RCON1
 6,AOX,BLDEF
 6,ATX,BLDEF
 ,ITA,
 13,UJ,
      *ISKK1A:6,XTA,RR2
 6,AAX,CON7
      6,AEX,CON7
 ,U1A,*ISKK1A-3
 *ISKK1B:,ITA,12
      ,ASN,64-24
      6,AEX,CON6
      6,STX,RR2
      6,ATX,RR1
 ,UJ,POISKL
C
C**************************************
C*
C*  CИHTAKCИЧECKИЙ KOHTPOЛЬ
C*  ИДEHTИФИKATOPA.
C*
C
 LOI:6,XTA,POSIND
 6,AEX,CON2
 ,ASN,64+1
 6,ARX,CON33
 6,XTS,POSIND
 6,AEX,FLAG3B
 ,ASN,64+1
 6,ARX,NGZB
 15,AEX,
 6,AAX,CON4
 6,AEX,CON4
 ,U1A,*+2
 6,XTA,POSIND
 12,UJ,
 :6,ATX,IMIST
 12,UJ,
C
C**************************************
C*
C*  OKOHЧAHИE ФOPMИPOBAHИЯ KOHCTAHTЫ И
C*  ЗAHECEHИE EE B TEЛO ПPOГPAMMЫ.
C*
C
 *MADEC1 :   ,  ITA  ,13
 6,XTS,FLAGTABL
 6,AAX,CON1
 13,VTM,*MACAF-1
 ,UZA,LEFCOM-1
 15,UTM,-1
 *MACAB:13,VTM,*ABP1A
 ,UJ,*VMET1
 *MADEC2:6,XTA,NK
 6,ARX,RCON1
 PLG6:6,ATX,NK
 6,AEX,NKMAX
 PLG7:13,U1A,
 ,UJ,FATNAME+2
 6,XTS,MESKOP
 6,XTS,MESADR4
 6,AAX,CONR24
 6,XTS,MESADR4
 ,ASN,64+24
 7,VJM,MADEC2A
 :6,XTA,MESKOP
 6,AEX,MESADR1
 6,STX,XADRESS1
 7,VJM,MADEC2A
 :6,XTA,MESKOP
 6,AEX,MESADR1
 6,STX,XADRESS2
 6,ATX,MESKOP
 6,XTA,XADRESS1
 13,VJM,COMMAND
 15,XTA,
 ,ATI,13
 :6,XTA,XADRESS2
 ,UJ,COMMAND
 MADEC2A :  6,  ATX  ,RR1
            6,  AAX  ,CN00002
             ,  UZA  ,*MACB2              KOPOTKИЙ AДPEC
            6,  XTA  ,RR1
            6,  AAX  ,CONKP1
 6,ATX,MESKOP
 6,AEX,RR1
 6,ATX,MESADR1
 6,AAX,CNR40000
 ,UJ,*ETAP3C4+1
 *MACB2  :  6,  XTA  ,RR1
            6,  AAX  ,CONR12
 6,ATX,MESADR1
 6,AEX,RR1
 6,ATX, MESKOP
 ,UJ,PARAM*+3
 :13,VTM,*FLAG2T
 ,UTC,
 *MACAF  :  6,  XTA  ,FLAGTABL
            6,  AAX  ,CON004
 ,U1A,*MACAF1
 6,XTA,ABSCNT
            6,  ARX  ,RCON1
            6,  ARX  ,BSSCNT
            6,  ATX  ,ABSCNT              ABCCNT=ABSCNT+BSSCNT+1
             ,  XTA  ,
            6,  ATX  ,BSSCNT              BSSCNT=0
           13,  UJ   ,
 *MACAF1:6,XTA,DATCNT
 6,ARX,RCON1
 6,ATX,DATCNT
 13,UJ,
C
C**************************************
C*
C*  ПEPEBOД BOCЬMEPИЧHЫX ЧИCEЛ B
C*  ДBOИЧHYЮ CИCTEMY.
C*
C
 PER6T8:6,ATX,RR1
 6,AAX,MASK*50
 6,XTS,RR1
 6,AEX,MASK*20
 6,ARX,MASK*17
 6,AAX,MASK*40
 15,AOX,
  6,ATX,RMIST
 6,XTA,RR1
 6,APX,MASK*07
 ,ASN,64+24
 13,UJ,
C
C**************************************
C*
C*  ПEPEBOД ЦEЛЫX ДECЯTИЧHЫX ЧИCEЛ B
C*  ДBOИЧHYЮ CИCTEMY.
C*
C
 PERI10:15,ATX,
 ,ASN,64-6
 ,UZA,PERI101
 15,XTA,-1
 :6,ARX,NG1B
 6,AAX,MASK*20
 15,AOX,-1
 6,AEX,MASK*20
 15,ATX,
 6,ARX,MASK*06
 15,AOX,
 6,AAX,MASK*60
 ,U1A,PERI10E
 15,XTA,
 ,ASN,64-6
 15,ATX,
 ,YTA,
 6,AEX,RCON20
 PERI10L:15,STX,2
 ,UZA,PERI101-1
 ,ASN,64-6
 15,ATX,
 ,YTA,
 6,AEX,RCON20
 15,ATX,
 15,XTA,1
 ,ASN,64-1
 15,ATX,
 ,ASN,64-2
 15,ARX,
 15,ARX,
 ,UJ,PERI10L
 :15,XTA,3
 13,UJ,
 PERI101:15,XTA,
 13,UZA,
 6,AEX,CON2
 15,ATX,
 6,ARX,CON66
 6,AAX,RCON1
 ,U1A,PERI10E
 15,XTA,
 ,ASN,64-6
 ,YTA,
 13,UJ,
 PERI10E :  6,  XTA  ,CON1
 6,STX,RMIST
             ,  XTA  ,
           13,  UJ   ,
C
C**************************************
C*
C*  OБPAБOTKA ПOЛHOГO AДPECA.
C*
C
 PADRESS2:,ITA,13
 6,ATX,*EQPLG
 ,XTS,
 1,XTS,TREP*C1-PER
 1,ATX,14
 13,VJM,PADRESSF
 :6,ATX,*EQPLG
 6,XTA,MESADR1
 6,XTS,MESADR3
 6,XTS,TSCF
 6,AEX,CON4
 13,VTM,REP*C1-2
 ,U1A,*READ3
 1,XTA,TSC0-PER
 1,ATX,10
 13,VJM,PADRESSF
 :6,XTA,MESADR3
 15,ATX,-3
 :6,XTA,MESADR1
 15,WTC,-4
 13,VTM,
 15,ATX,-4
 ,ITA,
 6,ATX,MESADR5
 6,ATX,XADRESS5
 15,UTM,-4
 13,UJ,
 :,XTA,
 6,ATX,MESADR1
 6,ATX,MESADR3
 ,UJ,*-6
 REP*C1:1,XTA,7
 1,ATX,14
 6,XTA,*EQPLG
 ,XTS,
 6,STX,*EQPLG
 ,UZA,*+4
 6,XTA,TSCF
 ,U1A,TO41
 6,XTA,CON4
 6,ATX,TSCF
 1,WTC,97
 ,UJ,
 :6,XTA,INDTABL
 ,U1A,TO41
 1,XTA,TSC0-PER
 1,ATX,10
 TREP*C1:,UJ,ABP
 ,UJ,REP*C1
 PADRESSF:6,XTA,RCON1
 ,UJ,PADRESSC+1
 PADRESSC:6,XTA,RCON1
 6,ATX,IDFLAG
 :6,ATX,IFFLAG
 ,UJ,PADRESS
 PADRESSI:6,XTA,RCON1
 6,ATX,IDFLAG
 PADRESS:,ITA,13
             ,  XTS  ,
            8,  VTM  ,
            6,  ATX  ,XADRESS1            PAБOЧИE ЯЧEЙKИ0
            6,  ATX  ,MESADR1
            6,  ATX  ,XADRESS2
            6,  ATX  ,MESADR2
            6,  ATX  ,MESADR3
 6,ATX,MESADR4
 6,ATX,BASYPR
 6,ATX,MESADR5
 6,ATX,XADRESS5
 6,ATX,MESADRB
 6,ATX,XADRESSB
 6,ATX,TSCF
 6,ATX,BLBSS
 6,ATX,MSTR
 6,ATX,MSTR+1
 6,ATX,FLGADR
 6,ATX,MSTR+3
 6,XTA,*EQPLG
 ,UZA,*+2
 1,XTA,TSC0-PER
 1,ATX,10
 *PAD:1,XTA,*C-PER
 1,ATX,13
 1,XTA,*C1-PER
 1,ATX,15
 13,VTM,ABP+1
 1,XTA,*C2-PER
 1,ATX,31
 1,XTA,*C3-PER
 1,ATX,9
 1,XTA,*C4-PER
            1,  ATX  ,97
 ,UJ,READA
 SUB     :  6,  XTA  ,IMIST               -
             ,  U1A  ,TO41
            6,  XTA  ,INDTABL
 11,VTM,ETAP2
 ,UZA,*SUB
 6,XTA,CON4
 6,ATX,XADRESS2
 ,UJ,SIMPLAD
 ADD:6,XTA,IMIST
 ,U1A,TO41
 6,XTA,INDTABL
 11,VTM,ETAP2
 ,U1A,SIMPLAD
 6,XTA,CON4
 *SUB:6,AEX,CON4
 6,ATX,XADRESS1
 ,XTA,
 6,ATX,XADRESS2
 TSC0:,UJ,ABP
 ,UJ,*+1
 :1,XTA,7
 1,ATX,10
 6,ATX,TSCF
 1,XTA,TSCC-PER
 1,ATX,11
 6,XTA,IMIST
 6,AOX,INDTABL
 13,VTM,ABP
 ,UZA,RELONG5
 ,UJ,TO41
 TO4     :  6,  XTA  ,IMIST               KOHEЦ ПOЛHOГO AДPECA
 11,VTM,ETAP3
 ,U1A,TO41
 6,XTA,INDTABL
 ,U1A,SIMPLAD
 6,ATX,XADRESS1
 11,UJ,
 SIMPLAD :  6,  XTA  ,INDTABL
 6,ATX,FLGADR
 ,ASN,64+6
 ,UTC,
 ,UZA,SIM1S
 6,XTA,INDTABL
 6,ATX,RR1
 13,VJM,*WRIN
 :6,XTA,POSIND
 6,AAX,CON7
 6,AEX,CON2
 ,UTC,
 ,UZA,SIM10
 6,XTA,POSIND
 6,AEX,CON2
 ,ASN,64+1
 6,ARX,CON33
 ,UZA,*SIM5
 :6,XTA,POSIND
 13,VTM,*SIM2
 15,ATX,
 ,UJ,PERI10+2
 *SIM2:6,ATX,RR1
 6,XTA,RMIST
 :,U1A,TO41
 6,XTS,XADRESS1
 :6,ARX,NGZB
 6,AEX,CONR16
 15,ATX,
 6,ARX,RR1
 15,AEX,
 6,ARX,MESADR3
 6,AAX,CONR15
 6,STX,MESADR3
 6,ATX,XADRESS1
 11,UJ,
 SIM1S:6,XTS,INDTABL
 6,ATX,RR1
 ,ASN,64+6
 ,YTA,
 6,STX,POSIND
 2,VTM,STROKA+INDTABL
 2,ATX,
 5,VTM,-8
 6,XTA,POSIND
 6,AEX,FLAG3B
 ,UZA,*SIM4
 6,XTA,POSIND
 6,AEX,CON2
 6,ARX,CON66
 6,AAX,RCON1
 ,U1A,*SIM5
 6,XTS,RR1
 6,AEX,RCON20
 6,STX,RR1
 ,UJ,*SIM2+1
 SIM10:6,XTA,RR1
 6,AAX,AA6SIMBR
 6,AEX,BSIMB
 ,U1A,*SIM2-2
 6,XTA,RR1
 ,ASN,64+6
 13,VTM,*SIM2
 ,UJ,PER6T8
 *SIM4:6,XTA,NK
 6,ATX,RR1
 6,XTA,CON1
 6,AEX,XADRESS1
 6,XTS,XADRESS1
 ,UJ,*SIM2+2
 *SIM5   :  6,  XTA  ,POSIND
           13,  VJM  ,ISKADR
 :6,ATX,RR1
 6,XTA,RR2
 ,ASN,64-3
 6,AAX,XADRESS1
 ,ASN,64-2
 ,YTA,
 6,AOX,BLDEF
 6,STX,BLDEF
 6,AEX,XADRESS1
 6,ATX,XADRESS1
 6,AAX,CONR15
 6,ATX,MESADR1
 6,AEX,XADRESS1
 6,ATX,XADRESS1
 6,AAX,CON4
 15,ATX,
 6,AEX,XADRESS1
 6,ARX,NGZB
 15,AEX,-1
 6,AAX,XADRESS1
 6,AEX,RR2
 6,STX,XADRESS1
 6,ARX,NGZB
 6,AEX,CONR15
 15,ATX,
 6,ARX,RR1
 15,AEX,
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 11,UJ,
 APO:6,ATX,FLGADR
 6,XTA,IMIST
 6,AOX,INDTABL
 13,VTM,*+2
 ,UZA,READ
 ,UJ,TO41
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-47B
 14,V1M,TO41
 ,XTA,
 6,ATX,RR1
 11,VTM,-7
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-47B
 14,VZM,APO1
 6,XTA,RR1
 ,ASN,64-3
 14,UTM,47B-60B
 ,ITS,14
 15,AEX,
 6,ATX,RR1
 :,ITA,14
 ,ASN,64+3
 ,U1A,TO41
 11,VLM,READ
 11,VTM,1
 13,VJM,READ
 :14,UTM,-47B
 14,VZM,APO1
 14,UTM,47B-60B
 ,UJ,*-4
 APO1:6,XTA,RR1
 6,AAX,CONR15
 6,ATX,MESADR3
 6,AEX,RR1
 ,U1A,TO41
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-47B
 14,VZM,ETAP2
 14,UTM,47B-177B
 14,VZM,ETAP3
 TSCC:,UJ,TO41
 ,UJ,*+1
 :1,XTA,7
 1,ATX,11
 6,XTA,CON4
 6,ATX,TSCF
 6,XTA,*EQPLG
 ,UZA,*READ3
 1,WTC,97
 ,UJ,
 EQU:6,ATX,FLGADR
 8,VZM,*EQU1
 1,XTA,*C5-PER
            1,  ATX  ,13
            1,  ATX  ,15
            1,  XTA  ,7
            1,  ATX  ,9
             ,  UJ   ,*EQU2
 *EQU1:1,XTA,*C15-PER
 1,ATX,13
 1,XTA,*C16-PER
 1,ATX,15
 *EQU2:1,XTA,*C17-PER
 1,ATX,97
 1,XTA,/2TOЧ-PER
 1,ATX,28
 1,ATX,49
 1,XTA,/RSHIFT-PER
 1,ATX,53
 6,ATX,OCTAL
 6,ATX,DECIMAL
 ,XTA,
 6,ATX,REISCS
 1,XTA,*C18-PER
 1,ATX,43
 1,XTA,*C19-PER
 1,ATX,52
 1,XTA,*C20-PER
 1,ATX,42
 1,XTA,/D/-PER
      1,ATX,38
             ,  UJ   ,ABP
 RHSIFT:6,XTA,NGZB
 6,ATX,SIGNE
 ,XTA,
 6,ATX,OCTAL
 13,VTM,*REOCT2
 ,UJ,*CODES1
 LLOG:6,XTA,RCON1
 6,ATX,REISCS
 6,XTA,IMIST
 6,AOX,INDTABL
 13,VTM,ABP
 ,UZA,*CODES1
 ,UJ,TO41
 RADD:1,XTA,*C9-PER
 15,ATX,
 13,VTM,CODE8
 ,UJ,*CODES1
 RSUB    : 13,  VTM  ,RADD                -
 RMIN    :  6,  XTA  ,CON4
 6,AEX,XADRESS2
            6,  ATX  ,XADRESS2
           13,  UJ   ,
 RTO4:1,XTA,*C10-PER
 15,ATX,
 13,VTM,CODE8
 ,UJ,*CODES1
 EQR:6,XTA,CON3
 6,ATX,RE*R
 :6,XTA,IMIST
 6,AOX,INDTABL
 ,U1A,TO41
 6,ATX,RR1
 6,ATX,RR2
 6,ATX,RR3
 6,ATX,IRT
 6,ATX,IRSS
 6,ATX,IRE
 6,ATX,IRSH
 6,ATX,IRPF
 6,ATX,IRP
 6,ATX,IRFLAG
 6,XTA,RE*R
 ,UZA,EQI0
 1,XTA,EQRT-PER
 1,ATX,16
 1,XTA,EQRE-PER
 1,ATX,39
 1,XTA,EQRP-PER
 1,ATX,50
 13,VJM,*CODES1+2
 EQI1:1,XTA,EQRAS-PER
 1,ATX,13
 1,ATX,15
 1,XTA,EQREND-PER
 1,ATX,97
 1,XTA,EQRS-PER
 1,ATX,53
 13,VJM,READ
 :,XTA,
  14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-53B
 6,ATX,IRS
 13,VTM,ABP+1
 14,VZM,READA
 14,UTM,-2
 :14,VZM,*+2
 14,UTM,55B
 14,WTC,ABP+2
 ,UJ,
 :6,XTA,CON002
 6,ATX,IRS
 EQRT:,UJ,READA
 ,UJ,*+1
 :1,XTA,/6/-PER
 1,ATX,16
 6,XTA,CON4
 13,VJM,EQRMULT
 :13,VTM,ABP+1
 ,UJ,READA
 :1,XTA,LOGPL-PER*3
 13,VTM,IRSH
 1,ATX,53
 14,VTM,
 EQRSEM:1,XTA,/6/-PER
 1,ATX,16
 6,XTA,IMIST
 ,U1A,TO41IR
 ,ITA,14
 6,ATX,TELLI
 6,XTA,IRFLAG
 ,ITS,13
 6,STX,IRFLAG
 ,UZA,PERILONG
 6,ATX,IRB
 6,XTA,INDTABL
 :,UZA,EQRSEM1
 13,VJM,*WRIN
 :6,XTA,POSIND
 13,VJM,PERI10
 :6,ATX,SIGN1
 6,AAX,MASK41
 6,AOX,RMIST
 ,U1A,TO41IR1
 6,WTC,IRB
 13,VTM,STROKA
 6,XTA,IRSS
 6,ARX,SIGN1
 6,AEX,IRSS
 13,ARX,
 6,AAX,CONR15
 13,ATX,
 EQRSEM1:6,XTA,TELLI
 ,U1A,EQREND1
 6,ATX,IRSS
 13,VJM,READ
 14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-53B
 13,VTM,ABP+1
 14,VZM,READA
 14,UTM,-2
 :14,VZM,*+2
 14,UTM,55B
 14,WTC,ABP+2
 ,UJ,
 :6,XTA,CONR15
 6,ATX,IRSS
 EQRE:,UJ,READA
 ,UJ,*+1
 :1,XTA,*C28-PER
 13,VTM,IRE
 1,ATX,39
 14,VTM,
 EQRP:,UJ,EQRSEM
 ,UJ,*+1
 :1,XTA,*C24-PER
 13,VTM,IRP
 1,ATX,50
 6,XTA,CINT0
 14,VTM,
 6,ATX,IRPF
 EQRAS:,UJ,EQRSEM
 ,UJ,*+1
 :8,V1M,TO41IR
 14,UTM,-53B
 14,VZM,*+1
 13,VJM,RMIN
 :13,VTM,ETAP2
 14,VTM,177B
 EQREND:,UJ,EQRSEM
 ,UJ,*+1
 :13,VTM,ETAP3
 ,UJ,EQRSEM
 TO41IR:,XTA,
 2,VTM,MULTI
 6,ATX,INDTABL
 13,VTM,TO41
 :1,XTS,*C28-PER
 1,ATX,39
 1,XTA,*C24-PER
 1,ATX,50
 1,XTA,LOGPL-PER*3
 1,STX,53
 ,NTR,7
 13,UJ,
 TO41IR1:,XTA,
 6,ATX,RR1
 6,ATX,RR2
 6,ATX,RR3
 :2,VTM,MULTI
 6,ATX,IMIST
 :6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 ,UJ,EQRSEM1
 EQRMULT:14,VTM,1-MULTI
 2,J+M,14
 14,J+M,14
 14,J+M,14
 14,J+M,14
 5,J+M,14
 ,ITS,14
 15,AEX,
 6,ATX,IRT
 13,UJ,
 PERILONG:6,XTA,INDTABL
 ,UZA,EQRSEM1
 2,XTA,
 6,AAX,AA6SIMBR
 6,AEX,BSIMB
 6,ATX,IRB
 :,U1A,*+5
 2,XTA,
 ,ASN,64+6
 2,ATX,
 5,UTM,-1
 ,U1A,*+3
 :6,XTA,INDTABL
 ,UZA,EQRSEM1
 2,UTM,-1
 5,VTM,
 :6,XTA,CON4
 6,AAX,IRT
 :13,VTM,*+1
 ,UZA,EQRMULT
 :2,MTJ,14
 13,VJM,*WRIN
 14,ATX,1
 6,XTA,POSIND
 6,ATX,INDTABL
 14,XTA,
 6,ARX,NG1B
 14,AEX,
 6,AAX,MASK*20
 14,AOX,
 14,ATX,
 6,AEX,MASK*20
 :,U1A,*+4
 14,ATX,
 6,XTA,INDTABL
 ,UZA,EQRSEM1
 14,XTA,-1
 6,AEX,MASK*20
 14,UTM,-1
 ,UJ,*-3
 :6,XTA,IRT
 ,ATI,12
 6,XTA,IRB
 ,UZA,PERIB
 PERID:2,XTA,
 6,AEX,MASK*20
 2,ATX,
 14,VTM,-7
 6,ARX,MASK*06
 2,AOX,
 6,AAX,MASK*60
 ,U1A,TO41IR
 PERID1:2,XTA,
 ,ASN,64-6
 2,ATX,
 ,YTA,
 12,UTM,-1
 13,VJM,PERI3
 ,UTC,
 ,U1A,TO41IR1
 :2,XTA,
 ,UZA,*+3
 :14,VLM,PERID1
 2,XTA,1
 ,UZA,*+2
 2,VLM,PERID
 :2,XTA,1
 ,U1A,*-2
 :,ITA,12
 6,ATX,IRE
 2,VTM,MULTI
 13,VTM,EQRSEM1
 PERI/:6,XTA,RR3
 ,UZA,*+2
 ,XTA,
 13,UJ,
 :6,XTS,RCON50
 6,ARX,IRSH
 6,AAX,CONR15
 6,ATX,IRSH
 6,XTA,RR2
 6,ATX,RR3
 6,XTA,RR1
 6,STX,RR2
 6,ATX,RR1
 ,UJ,PERI/
 PERI3:6,XTS,RR3
 ,ASN,64-1
 15,ATX,
 ,ASN,64-2
 15,ARX,
 15,ARX,
 6,ATX,RR3
 6,AAX,CON776
 6,AEX,RR3
 6,ATX,RR3
 ,YTA,
 ,ASN,64-8
 ,YTA,
 6,XTS,RR2
 ,ASN,64-1
 15,ATX,
 ,ASN,64-2
 15,ARX,
 15,ARX,
 6,ATX,RR2
 6,AAX,CON776
 6,AEX,RR2
 6,ATX,RR2
 ,YTA,
 ,ASN,64-8
 ,YTA,
 6,XTS,RR1
 ,ASN,64-1
 15,ATX,
 ,ASN,64-2
 15,ARX,
 15,ARX,
 6,ATX,RR1
 6,AAX,CON776
 13,UJ,
 PERIB:2,XTA,
 6,AEX,MASK*20
 2,ATX,
 14,VTM,-7
 6,AAX,MASK*70
 ,U1A,TO41IR1
 PERIB1:2,XTA,
 ,ASN,64-6
 2,ATX,
 ,YTA,
  12,UTM,-1
 6,XTS,RR3
 ,ASN,64-3
 15,AEX,
 6,ATX,RR3
 6,AAX,CON776
 6,AEX,RR3
 6,ATX,RR3
 ,YTA,
 ,ASN,64-8
 ,YTA,
 6,XTS,RR2
 ,ASN,64-3
 15,AEX,
 6,ATX,RR2
 6,AAX,CON776
 6,AEX,RR2
 6,ATX,RR2
 ,YTA,
 ,ASN,64-8
 ,YTA,
 6,XTS,RR1
 ,ASN,64-3
 15,AEX,
 6,ATX,RR1
 6,AAX,CON776
 ,UTC,
 ,U1A,TO41IR1
 :2,XTA,
 ,UZA,*+3
 :14,VLM,PERIB1
 2,XTA,1
 ,UZA,*+2
 2,VLM,PERIB
 2,XTA,1
 ,U1A,*-2
 :,ITA,12
 12,J+M,12
 ,ITS,12
 15,ARX,
 6,AAX,CONR15
 2,VTM,MULTI
 6,ATX,IRSH
 13,VTM,EQRSEM1
 EQRS:,UJ,PERI/
 ,UJ,EQRSEM-2
 :6,XTS,RCON50
 6,ARX,IRSH
 6,AAX,CONR15
 6,ATX,IRSH
 6,XTA,RR2
 6,ATX,RR3
 6,XTA,RR1
 15,AEX,-1
 6,STX,RR2
 ,ASN,64-8
 ,YTA,
 6,ATX,RR1
 13,UTM,-1
 ,UJ,EQRSHIFT-1
 EQREND1:6,XTA,IRPF
 ,UZA,*+6
 6,XTA,IRP
 6,ARX,CONR15
 6,AEX,CONR15
 6,ARX,RCON150
 6,AAX,CONR15
 ,ASN,64+7
 15,ATX,
 ,YTA,
 6,STX,IRP
 ,U1A,TO41IR
 :6,XTA,RR3
 ,UZA,EQRSHIFT
 6,XTA,IRE
 13,VTM,EQRSHIFT
 ,UZA,EQRNORM
 ,ATI,12
 6,AAX,CNR40000
 ,U1A,EQRDIV
 :12,UTM,-1
 13,VJM,PERI3
 ,U1A,EQRS+1
 13,UTM,-1
 :12,V1M,PERI/
 13,VJM,EQRNORM
 EQRSHIFT:6,XTA,RCON150
 6,ARX,IRSH
 :,ATI,14
 6,XTA,RR1
 ,NTR,6
 ,UZA,CCCC
 6,ATX,RR2
 ,ITA,14
 ,ASN,64+7
 15,ATX,
 ,YTA,
 6,AEX,RR1
 6,STX,RR1
 ,UZA,CCCC
 ,ITA,14
 6,ARX,CONR15
 6,AEX,CONR16
 ,ATI,14
 ,ASN,64+7
 ,UTC,
 ,U1A,TO41
 6,XTA,RR2
 14,ASN,64
 6,ATX,RR1
 CCCC:6,XTA,IRP
 ,UZA,EQRSIGN
 ,ASN,64+1
 6,AEX,CON776
 6,XTS,RR1
 ,ASN,64+1
 15,ARX,
 ,U1A,TO41IR
 6,XTA,RR1
 ,NTR,3
 6,A+X,IRP
 6,ATX,RR1
 EQRSIGN:6,XTA,RR1
 6,AVX,IRS
 13,VTM,CODER
 ,UJ,TO41IR+2
 EQRDIV:12,UTM,1
 13,VJM,EQRNORM
 :13,VTM,-23
 ,ITA,13
 6,ARX,IRE
 6,ARX,IRSH
 6,ATX,IRSH
 6,XTA,CON774
 6,AEX,RR1
 6,ATX,RR1
 6,XTA,CON534
 6,AEX,RR2
 ,NTR,6
 ,E+N,100B
 CYDIV:6,ATX,RR2
 6,XTA,RR1
 6,A*X,C/5
 6,ATX,-1
 ,YTA,30B
 6,ATX,-2
 6,XTA,RR2
 6,A*X,C/5
 6,ATX,-3
 ,YTA,30B
 6,ATX,-4
 6,XTA,RR1
 6,A*X,C//5
 6,ATX,-5
 ,YTA,30B
 6,ATX,-6
 6,XTA,RR2
 6,A*X,C//5
 6,ATX,-7
 ,YTA,31B
 6,A+X,-7
 6,A+X,-6
 6,A+X,-4
 ,E+N,101B
 6,A+X,-5
 6,A+X,-3
 6,A+X,-2
 6,A+X,-1
 6,ATX,RR1
 ,YTA,30B
 12,VLM,CYDIV
 15,ATX,
 6,ARX,CON001
 6,ARX,CON002
 15,A+X,
 6,A+X,RR1
 ,ASN,64-7
 6,ATX,RR1
 ,YTA,
 6,XTS,RR1
 ,ASN,64+7
 6,STX,RR1
 6,ARX,IRSH
 ,UJ,EQRSHIFT+1
 EQRNORM:6,XTA,RE*R
 6,AEX,CON2
 14,VTM,80
 ,U1A,EQRNORM1
 6,XTA,RR2
 6,AOX,RR1
 ,U1A,TO41IR
 6,XTA,RR3
 6,ANX,RCN77766
 6,AEX,CONR15
 ,ATI,14
 6,ARX,IRSH
 6,ATX,IRSH
 6,XTA,RR3
 14,ASN,64
 6,ATX,RR1
 13,UJ,
 :6,XTS,RR2
 6,ATX,RR1
 6,XTA,RR3
 6,STX,RR2
 6,ATX,RR3
 14,UTM,-40
 EQRNORM1:6,XTA,RR1
 ,UZA,*-3
 :6,ANX,RCN77766
 6,AEX,CONR15
 ,ITS,14
 15,ARX,-1
 6,ARX,IRSH
 6,STX,IRSH
 ,ATI,14
 6,XTA,RR1
 14,ASN,64
 6,ATX,RR1
 6,XTA,RR2
 14,ASN,64-8
 15,ATX,
 ,YTA,
 6,AEX,RR1
 6,STX,RR1
 ,ASN,64+8
 6,ATX,RR2
 6,XTA,RR3
 14,ASN,64-8
 ,YTA,
 6,AEX,RR2
 6,ATX,RR2
 13,UJ,
 EQI:,XTA,
 6,ATX,RE*R
 13,VTM,EQR+1
 ,UJ,*CODES1+1
 EQI0:6,XTA,CINT0
 6,ATX,IRPF
 6,XTA,CON2
 6,ATX,RE*R
 ,UJ,EQI1
 :6,XTA,*PROBEL
 6,ATX,POSIND
 *EQHA:,UJ,*EQHEND
 EQH:6,XTA,INDTABL
 ,ASN,64+5
 TOTO41:6,AOX,IMIST
 ,U1A,TO41
 6,XTA,INDTABL
 13,VTM,*EQHA-1
 ,UZA,*CODES1
 6,AEX,RCON20
 6,ARX,RCON1
 15,ATX,
 6,AEX,CONR15
 ,STI,12
 ,ASN,64+3
 12,UTM,3
 ,U1A,TO41
 2,ATX,
 5,VTM,-48
 13,VJM,*CODES1
 :12,MTJ,7
 13,VJM,READ
 *EQH1:14,MTJ,12
 12,UTM,-177B
 12,VZM,TO41
 12,UTM,177B-47B
 12,V1M,*+2
 6,XTA,CON44
 :1,XTS,TOTO41-PER
 13,VJM,APOSTLIT
 :6,XTS,POSIND
 ,ASN,64-8
 5,UTM,8
 15,AEX,
 6,ATX,POSIND
 6,XTA,COMMA
 ,UZA,*+2
 6,XTA,CON44
 7,VLM,*-5
 ,UJ,TO41
 :13,VTM,*EQH1
 7,VLM,READ
 6,XTA,POSIND
 6,AEX,*PROBEL
 5,ASN,64
 6,AEX,*PROBEL
 6,ATX,POSIND
 5,VTM,-8
 *EQHEND:11,VTM,ETAP3
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 ,ITA,11
 14,UTM,40B-177B
 14,VZM,CODES-1
 14,UTM,177B-56B
 14,V1M,*+2
 13,VJM,EDITTT
 :,ITA,11
 ,UJ,CODES-1
 :1,XTA,*C9-PER
 8,V1M,TO41
 14,UTM,56B-53B
 14,VZM,CODES-1
 14,UTM,53B-55B
 14,V1M,TO41
 6,XTS,CON4
 6,STX,XADRESS2
 :6,XTS,CON4
 6,XTS,POSIND
 CODES   :  6,  XTS  ,IMIST
 ,U1A,*+9
 15,XTA,
 13,VJM,ISKKOS
 :15,WTC,-2
 11,VTM,
 6,ATX,RR1
 6,XTA,XADRESS1
 ,ASN,64-1
 ,YTA,
 ,ASN,64-1
 6,AOX,BLDEF
 6,STX,BLDEF
 6,AEX,CON04
 6,AEX,XADRESS1
 15,ATX,-1
 6,AAX,CON4
 ,UJ,*SIM2+2
 :15,UTM,-3
             ,  UJ   ,TO41
 DOUBLE:6,XTA,IMIST
 6,AOX,INDTABL
 ,U1A,TO41
      13,VJM,*CODES1
      ,XTA,
      6,ATX,-1
      6,ATX,-2
 13,VJM,RELONG5
 DOUB0:,XTA,
 6,ATX,-3
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,-60B
 ,ITA,14
 ,ASN,64+3
 ,U1A,DSTOP
 ,ITA,14
 6,XTS,-1
 ,ASN,64-3
 15,AEX,
      6,ATX,-1
      6,XTA,-3
 6,ARX,RCON1
 6,ATX,-3
 6,AEX,RCON20
 ,U1A,READ
 6,XTA,-1
 6,XTS,-2
 6,ATX,-1
 13,VTM,DOUB4
 ,U1A,READ
      6,XTA,CON1
      6,ATX,-2
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-52B
 14,VZM,DOUB0
 :15,XTA,
 6,ATX,-3
 14,UTM,52B-177B
 1,XTA,*C10-PER
 14,VZM,*+6
 14,UTM,177B-56B
 14,VZM,DOUB4+3
 8,V1M,TO41
 1,XTA,*C9-PER
 14,UTM,56B-53B
 14,VZM,*+3
 14,UTM,53B-55B
 14,V1M,DOUB4+5
 6,XTS,CON4
 6,AEX,XADRESS2
 6,STX,XADRESS2
 :6,XTS,-3
 6,XTS,CON7
 6,XTS,-1
 ,UJ,CODES
 DSTOP:6,XTA,-2
 ,U1A,*-10
 6,XTA,-1
 ,XTS,
 6,ATX,-1
 14,UTM,60B-52B
 14,V1M,DSTOP-10
 6,XTA,CON1
 6,ATX,-2
 ,UJ,DOUB0
 DOUB4:14,UTM,-40B
 14,VZM,READ
 15,XTA,
 6,ATX,-1
 14,UTM,40B-52B
 ,UJ,DSTOP-10
 :15,ATX,
 13,VJM,EDITTT
 :15,XTA,
 ,UJ,DSTOP-2
 :14,WTC,ABP+57B
 14,VTM,
 ,ITA,14
 ,UJ,DSTOP-2
 CODE8:1,XTA,*C38-PER
 6,ATX,OCT*PLG
 6,XTA,REISCS
 ,U1A,*REOCT1
 6,XTA,OCTAL
 ,UZA,SHITS
 1,ATX,53
 ,UJ,*REOCT2
 SHITS:6,XTA,DECIMAL
 ,UZA,SHITS1
 6,XTA,INDTABL
 ,U1A,SHITS1
 14,UTM,-177B
 14,VZM,SHITS1
 6,STX,DECIMAL
 6,XTA,CON4
 6,AAX,XADRESS2
 6,XTS,XADRESS2
 15,AEX,-1
 6,ATX,XADRESS2
 6,XTA,SIGNE
 15,ARX,-1
 15,AEX,
 6,ATX,SIGNE
 ,UJ,ABP
 CODER:6,ATX,RR1
 6,XTA,IRFLAG
 6,XTS,RE*R
 6,XTS,CODPLG
 ,U1A,*+2
 6,XTA,RR1
 *C38:,UJ,CODES
 ,UJ,*CODE8
 :,ATI,13
 6,XTA,RR1
 15,UTM,-3
 13,UJ,
 ETAP2:6,XTA,XADRESS1
 ,U1A,*+4
 :6,XTS,XADRESS2
 6,STX,XADRESS1
 6,ATX,XADRESS2
 6,ATX,MESADR2
 6,ATX,MESADR1
 ,UJ,*PAD
 :6,XTA,MSTR
 ,U1A,*+4
 6,XTA,XADRESS1
 6,ATX,MSTR
 6,XTA,MESADR1
 6,ATX,MSTR+2
 ,ITA,
 ,UJ,ETAP2+1
 6,XTA,MSTR+1
 ,U1A,TO41
 6,XTA,XADRESS1
 6,ATX,MSTR+1
 6,XTA,MESADR1
 6,ATX,MSTR+3
 ,ITA,
 ,UJ,ETAP2+1
 ETAP3:6,XTA,MSTR
 ,UTC,
 ,U1A,*+2
 6,ATX,XADRESS2
 :6,ATX,MESADR2
 ,UJ,ETAP3*
 :6,XTA,XADRESS1
 ,UZA,*+4
 6,XTA,MSTR+1
 ,U1A,TO41
 6,XTA,MSTR
 6,ATX,XADRESS2
 6,XTA,MSTR+2
 ,UJ,ETAP3+2
 :6,XTA,MSTR
 6,ATX,XADRESS1
 6,XTA,MSTR+2
 6,ATX,MESADR1
 6,XTA,MSTR+1
 6,ATX,XADRESS2
 6,XTA,MSTR+3
 6,ATX,MESADR2
 ETAP3*:6,XTA,MISTABL
 6,AAX,CON02
 ,U1A,TO41
 6,XTA,BLDEF
 ,UZA,*ETAP3
 6,ATX,BLBSS
 6,XTA,XADRESS1
 ,ASN,64-1
 6,AEX,CON4
 6,ARX,NGZB
 6,XTS,XADRESS2
 ,ASN,64-1
 6,AEX,CON4
 6,ARX,NGZB
 15,AOX,
 6,AAX,RCON2
 6,AOX,BLDEF
 6,ATX,BLDEF
 ,ASN,64+1
 6,AOX,BLUTC1
 6,ATX,BLUTC1
 ,U1A,ETAP3**
 6,XTA,BASPR
 ,UZA,ETAP3**
 6,XTA,MESMOD
 ,UZA,ETAP3**
 6,XTA,XADRESS1
 6,AAX,CON04
 6,ATX,RR1
 6,AEX,XADRESS1
 6,ATX,XADRESS1
 6,XTA,RR1
 6,ARX,NGZB
 6,AOX,CON4
 6,AAX,XADRESS1
 6,XTS,RR1
 ,ASN,64-1
 15,AEX,
 6,XTS,XADRESS2
 6,AAX,CON04
 6,ATX,RR1
 6,AEX,XADRESS2
 6,ATX,XADRESS2
 6,XTA,RR1
 6,ARX,NGZB
 6,AOX,CON4
 6,AAX,XADRESS2
 15,AEX,
 6,XTS,RR1
 ,ASN,64-1
 15,AEX,
 6,AEX,CON4
 PLG8:,XTS,
 6,ATX,MESADR1
 6,ATX,MESADR2
 6,ATX,MESADR3
 6,XTA,CON1
 6,ATX,MESADR4
 6,ATX,XADRESS5
 6,ATX,XADRESS1
 6,STX,BASYPR
 ,UZA,ENDADR*
 6,AEX,CON5
 ,UZA,*+2
 6,XTA,BASYSL
 ,UZA,ENDADR*
 :6,XTA,MISTABL
 6,AAX,CN0002
 14,VTM,ENDADR*
 ,U1A,*+5
 6,XTA,GLOB*
 ,U1A,STOLONG
 :6,XTA,BASEMOD
 14,UZA,
 6,XTA,XBASE
 6,AEX,YSLUTC
 14,UZA,
 ,UJ,STOLONG
 :6,XTA,BASLONG
 14,UZA,
 ,UJ,*-4
 ETAP3**:6,XTA,XADRESS1
 6,AAX,CON7
 6,ATX,XADRESS1
 6,XTA,XADRESS2
 6,AAX,CON7
 6,ATX,XADRESS2
 ,XTA,
 ,UJ,PLG8
 :6,AEX,CON5
 ,UZA,*ETAP3
 6,XTA,BASYSL
 ,U1A,*ETAP3
 :6,XTA,CON4
 6,ATX,BLUTC1
 *ETAP3:6,XTA,XADRESS1
 6,AOX,XADRESS2
 ,UZA,*ETAP3C
 6,XTA,XADRESS1
 ,UZA,*ETAP3WZ+1
 15,UTC,1
 8,VTM,
 6,AEX,MESADR1
 8,ATX,
 6,XTA,XADRESS2
 ,U1A,*+2
 6,XTS,XADRESS1
 6,STX,XADRESS5
 ,UJ,*ETAP3WZ+4
 :6,AEX,MESADR2
 8,ATX,1
 8,AEX,
 ,U1A,*+2
 :6,XTA,CON2
 ,UJ,*ETAP3W1+2
 :6,AEX,CON4
 7,VTM,
 ,U1A,*+3
 6,ATX,XADRESS1
 6,ATX,XADRESS2
 6,ATX,MESADR1
 6,ATX,MESADR2
 ,UJ,*ETAP3C
 :,ITA,
 8,ATX,2
 :8,MTJ,14
 14,UTM,2
 *ETAP31:8,XTA,
 ,UZA,*ETAP32
 6,AAX,CON2
 8,UTM,1
 :,UZA,*ETAP31
 8,WTC,-1
 12,VTM,TABDEC-4000B
 12,XTA,
 6,AAX,CONSS2
 ,UZA,*+8
 6,AEX,TOP
 ,U1A,*ETAP31
 8,XTA,-1
 6,AAX,CON4
 6,AEX,CON1
 8,ATX,-1
 ,ASN,64+7
 12,XTS,
 ,NTR,7
 15,AVX,
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 ,UJ,*ETAP31
 :14,ATX,1
 12,XTA,
 ,ASN,64+24
 8,AEX,-1
 6,AAX,CONR15
 8,AEX,-1
 8,ATX,-1
 12,XTA,
 6,AEX,CNR4000
 6,AAX,CONR15
 12,XTS,
 6,AAX,CN000002
 ,ASN,64+17
 ,YTA,
 8,AEX,-1
 6,AAX,CON6
 15,AEX,
 14,ATX,
 14,VLM,*ETAP31+2
 *ETAP32:15,UTC,1
 8,VTM,
 8,XTA,2
 ,UZA,*ETAP31-7
 :8,MTJ,14
 14,XTA,1
 8,UTM,1
 ,UZA,*ETAP33
 :8,AEX,-1
 6,AEX,CON4
 ,UZA,*+2
 14,XTA,2
 ,UZA,*-4
 14,VLM,*-2
 :14,ATX,1
 8,MTJ,14
 :14,XTA,
 14,ATX,-1
 14,UTM,1
 ,U1A,*-1
 :14,XTA,
 14,ATX,-2
 14,UTM,1
 ,U1A,*-1
 15,XTA,1
 ,UZA,*ETAP31-5
 15,XTA,2
 8,UTM,-1
 ,U1A,*ETAP32+2
 15,ATX,3
 *ETAP33:15,UTC,1
 8,VTM,
 8,XTA,2
 ,U1A,*+7
 8,XTA,
 6,AAX,CON7
 6,ATX,XADRESS1
 8,AEX,
 6,ATX,MESADR1
 8,XTA,1
 6,AAX,CON7
 6,ATX,XADRESS2
 ,UZA,*ETAP3WZ+4
 8,AEX,1
 6,ATX,MESADR2
 ,UJ,*ETAP31-7
 :7,VZM,*ETAP31-7
 7,UJ,
 ADR*R:6,XTA,XADRESS1
 6,AAX,CON1
 ,UZA,*+3
 6,ARX,XADRESS1
 ,XTS,
 13,VJM,SSILKR
 :6,STX,MESADR1
 6,ATX,XADRESS1
 :6,XTA,XADRESS2
 6,AAX,CON1
 ,UZA,*+3
 6,ARX,XADRESS2
 ,XTS,
 13,VJM,SSILKR
 :6,STX,MESADR2
 6,ATX,XADRESS2
 ADRNORM:6,XTA,XADRESS1
 6,AEX,MESADR1
 6,AAX,NCON4
 6,AEX,NCON4
 6,XTS,XADRESS2
 6,AEX,MESADR2
 6,AAX,NCON4
 15,ARX,
 8,U1A,
 6,XTA,MESADR1
 6,XTS,MESADR2
 6,STX,MESADR1
 6,ATX,MESADR2
 6,XTA,XADRESS2
 6,XTS,XADRESS1
 6,STX,XADRESS2
 6,ATX,XADRESS1
 8,UJ,
 ADR*RA:6,XTA,XADRESS1
 6,AAX,CON1
 ,UZA,*+3
 6,ARX,XADRESS1
 6,XTS,MESADR3
 13,VJM,*ETAP3WZ-2
 :6,STX,MESADR1
 6,ATX,XADRESS1
 :6,XTA,XADRESS2
 6,AAX,CON1
 ,UZA,ADRNORM
 6,ARX,XADRESS2
 13,VTM,ADRNORM-1
 6,XTS,MESADR3
 :,ITS,
 6,STX,MESADR3
 ,UJ,SSILKRR
 *ETAP3WZ:6,XTA,XADRESS1
 ,U1A,*ETAP3W1
 :6,XTS,XADRESS2
 6,ATX,XADRESS1
 6,ATX,XADRESS5
 6,XTA,MESADR2
 6,STX,MESADR1
 6,ATX,XADRESS2
 :6,ATX,MESADR2
 6,XTA,MESADR3
 6,ATX,MESADR5
 6,XTA,BASPR
 ,U1A,*ETAP3W2
 6,XTA,CON1
 6,ATX,BASYPR
 6,XTA,IFFLAG
 ,UZA,*ETAP3W3
 6,ATX,BLBSS
 6,XTA,XADRESS1
 6,AAX,CON1
 ,U1A,ENDAIF+4
 6,WTC,MESADR1
 14,VTM,TABDEC-4000B
 14,XTA,
 6,AAX,CONSS2
 ,U1A,ENDAIF+4
 14,XTA,
 ,ASN,64+24
 6,ATX,MESADR1
 14,XTA,
 6,AAX,CN000002
 ,ASN,64+17
 ,YTA,
 6,AEX,XADRESS1
 6,ATX,XADRESS2
 14,XTA,
 6,AAX,CONR15
 6,AEX,CNR4000
 6,ATX,MESADR2
 6,WTC,MESADR1
 14,VTM,TABDEC-4000B
 6,WTC,MESADR2
 12,VTM,TABDEC-4000B
 14,XTA,
 6,AEX,TOP
 ,U1A,*+3
 6,ATX,MESADR1
 6,XTA,CON3
 6,AEX,XADRESS1
 6,ATX,XADRESS1
 :12,XTA,
 6,AEX,TOP
 ,UTC,
 ,U1A,ENDAIF
 6,ATX,MESADR2
 6,XTA,CON3
 6,AEX,XADRESS2
 6,ATX,XADRESS2
 ENDAIF:6,XTA,IDFLAG
 14,VTM,*+8
 ,UZA,IFFORM
 14,VJM,IFFORM
 :6,ATX,BASE**
 8,VJM,ADR*R
 :13,VTM,*+4
 ,UJ,SSILKSD
 :6,XTA,IDFLAG
 6,ARX,NGZB
 6,AEX,IDFLAG
 6,AAX,BASE**
 6,ATX,BASE**
 6,XTA,MESADR1
 :6,AEX,XADRESS1
 ,UTC,
 :6,ATX,MESADR1
 ,UJ,ENDADR*
 IFFORM:6,XTA,XADRESS1
 6,AEX,XADRESS2
 ,ASN,64+3
 6,ATX,-1
 6,XTA,MESADR1
 6,AEX,MESADR2
 ,ASN,64-12
 6,AEX,-1
 6,ATX,-1
 8,VJM,ADRNORM
 :6,XTA,MESADR1
 6,AEX,-1
 6,AEX,XADRESS1
 14,UJ,
 *ETAP3W1:6,ATX,XADRESS5
 6,XTA,XADRESS2
 ,UZA,*ETAP3WZ+4
 6,XTA,CON2
 :6,ATX,XADRESS5
 6,XTA,MESADR3
 6,ATX,MESADR5
 6,XTA,BASPR
 ,U1A,*ETAP3W2
 6,XTA,CON1
 6,ATX,BASYPR
 6,XTA,RCON1
 6,ATX,BLBSS
 6,XTA,IFFLAG
 ,U1A,ENDAIF
 6,XTA,IDFLAG
 PLG72:,UZA,PARAMX+3
 8,VJM,ADR*R
 :7,VTM,ENDADR*
 13,VJM,SSILKSD
 :6,ATX,MESADR1
 ,UJ,ENDAID+2
 :6,XTA,MISTABL
 6,AAX,CN0002
 14,VTM,ADR*RA
 8,VTM,*+3
 ,UZA,PARNORM
 8,VJM,ADR*RA
 :13,VTM,ENDADR6+3
 ,UJ,SSILKSD
 :8,VTM,PARAMX
 13,VJM,SSILKSD
 ADR1*0:6,ATX,MESADR1
 6,XTA,MESADR3
 8,MTJ,13
 ,U1A,SSILKRD+2
 :6,XTA,MESADR1
 13,UJ,
 *ETAP3RL:6,XTA,XADRESS1
 ,NTR,12B
 :,UZA,*+4
 6,XTA,MESADR3
 :6,ARX,CONRL
 ,UZA,*+5
 6,XTA,CNR40000
 6,AEX,MESADR3
 :6,ATX,MESADR1
 7,UJ,
 :6,XTA,MESADR3
 13,VJM,SSILKNR
 :6,AEX,CNR7S12
 ,UJ,*ETAP3RL+4
 :6,XTA,MESADR3
 13,VJM,SSILKR
 :6,AEX,CNR7S12
 ,UJ,*ETAP3RL+4
 ENDADR5:6,XTA,BLDEF
 ,U1A,ENDADR6
 6,XTA,XADRESS1
 6,ATX,XADRESS5
 :6,XTA,MESADR3
 6,ATX,MESADR5
 :6,XTA,XADRESS1
 7,UZA,
 ENDAID:6,AAX,CON2
 13,VTM,*ETAP3C0+1
 ,UTC,
 ,UZA,SSILKRI
 :6,XTA,MISTABL
 6,AAX,CN0002
 ,U1A,*+5
 13,VJM,SSILKRD
 :6,ATX,MESADR1
 6,ARX,XADRESS1
 7,U1A,
 6,XTA,CON2
 6,ATX,XADRESS1
 13,VTM,*ETAP3C0+1
 ,UJ,ENDADR6+6
 :6,XTA,MESADR1
 ,ASN,64-24
 6,XTS,XADRESS1
 6,AAX,CON4
 6,ARX,NGZB
 6,AEX,CONR16
 15,ATX,
 6,ARX,MESADR3
 15,AEX,
 6,AAX,CONR15
 15,AEX,
 ,ITS,
 6,STX,MESADR3
 6,ATX,MESADR1
 7,UJ,
 ENDADR6:,XTA,
 ,UJ,*ETAP3C0+1
 6,AAX,CON1
 ,U1A,*ETAP3RL
 6,XTA,MESADR1
 6,AEX,XADRESS1
 :6,ATX,MESADR1
 13,VJM,SSILKRD
 :6,ATX,MESADR1
 6,ARX,XADRESS1
 13,VTM,*ETAP3RL+6
 ,U1A,ADR1*0+2
 :6,XTA,MESADR1
 6,AEX,CONNG
 6,ATX,-1
 ,UJ,SSILKD+1
 :6,XTA,MESADR3
 6,ATX,MESADR1
 *ETAP3C4:6,XTA,MESADR1
 6,AAX,CNR40000
 7,UZA,
 6,XTA,MESADR1
 13,VTM,*ETAP3RL+6
 ,UJ,SSILKC
 KOPNORM:6,XTA,MESKOP
 6,AAX,CN00001
 ,UZA,*+7
 6,AEX,MESKOP
 6,ATX,MESKOP
 6,XTA,CNR7S12
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 6,ATX,MESADR5
 6,XTA,XADRESSB
 ,UZA,*+3
 6,XTA,MESADRB
 6,ARX,CNR7S12
 6,AAX,CONR15
 6,ATX,MESADRB
 :6,XTA,MESADR3
 14,UJ,
 PARNORM:6,XTA,MESKOP
 15,ATX,
 6,AAX,CONKP1
 6,STX,MESKOP
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 6,ATX,MESADR5
 14,UJ,
 PARAM*:6,AAX,CONR12
 6,ATX,MESADR1
 6,AEX,MESADR3
 6,AEX,MESKOP
 6,ATX,MESKOP
 ,UTC,
 :6,AAX,CN00001
 13,VTM,*ETAP3C0+6
 ,U1A,*+4
 6,XTA,MESADR1
 :6,AAX,CNR4000
 7,UZA,
 *ETAP3C0:6,XTA,MESADR1
 13,VJM,SSILKC
 :6,ATX,MESADR1
 7,UJ,
 :6,XTA,GLOB*
 ,U1A,*ETAP3CA
 6,XTA,MESADR1
 6,AAX,CNR4000
 7,UZA,
 13,VJM,*ETAP3CA
 :13,VTM,*ETAP3C0+1
 ,UJ,SSILKCC
 13,VTM,*ETAP3C0+1
 ,UJ,SSILKC
 PARAMX:6,XTS,XADRESS1
 ,NTR,13B
 ,UZA,*+3
 15,XTA,
 :6,AEX,C52104S9
 13,VJM,SSILKEXD+1
 :6,ATX,MESADR1
 ,UJ,ENDADR
 :6,XTA,CONNG
 15,AEX,
 13,VTM,PARAMX+2
 ,UJ,SSILKEXD+1
 LONGER:6,XTA,MISTABL
 6,AOX,CON4
 6,ATX,MISTABL
 13,VJM,LONGAD
 :13,VTM,TO41+3
 ,UJ,*WRI
 *ETAP3C:6,XTA,CON1
 6,ATX,BASYPR
 6,XTA,MESADR3
 6,ATX,MESADR5
 6,XTA,IFFLAG
 6,AOX,IDFLAG
 7,VTM,ENDADR
 ,U1A,ENDADR*
 6,ATX,XADRESS5
 6,XTA,MESADR5
 PLG9:6,AAX,CNR7S12
 7,UZA,
 6,XTA,MISTABL
 6,AAX,CN0002
 6,AEX,BLUTC1
 7,U1A,
 6,XTA,BASPR
 7,UZA,
 6,XTA,MESKOP
 6,AAX,CN00001
 :,UZA,*+1
 6,XTA,CNR7S12
 :6,ARX,MESADR5
 6,ATX,-1
 6,AAX,CNR7S12
 7,UZA,
 6,AEX,CNR7S12
 7,UZA,
 6,XTA,MESMOD
 6,AOX,BASIR
 7,MTJ,14
 ,U1A,STOLONG
 6,XTA,GLOB*
 7,U1A,
 6,XTA,BASEMOD
 ,UZA,STOLONG
 6,XTA,XBASE
 6,AEX,YSLUTC
 ,U1A,STOLONG
 6,XTA,BASE*
 6,ARX,-1
 6,AAX,CNR7S12
 7,UZA,
 6,AEX,CNR7S12
 7,UZA,
 ,UJ,STOLONG
 PASS2C:6,XTA,MISTABL
 6,AAX,CN0002
 ,U1A,*ETAP3C2
 6,XTA,BASPR
 14,VTM,PARAM*
 ,UZA,PARNORM
 6,XTA,BLDEF
 14,VTM,*ETAP3CB
 ,UZA,KOPNORM
 6,XTA,BLUTC1
 ,U1A,KOPNORM
 6,XTA,CON1
 6,ATX,BLUTC1
 6,XTA,MESMOD
 6,AOX,BASIR
 ,UZA,KOPNORM
 6,XTA,GLOB*
 14,VTM,GLOB***-1
 ,U1A,KOPNORM
 6,XTA,BASEMOD
 14,VTM,*ETAP3CB
 ,UZA,KOPNORM
 6,XTA,XBASE
 6,AEX,YSLUTC
 ,UZA,KOPNORM
 14,VJM,KOPNORM
 :6,ATX,MESADR1
 14,VJM,STOLONG
 :,XTA,
 6,ATX,RR2
 6,XTA,MESADR1
 6,ARX,M33S47
  14,VTM,*ETAP3CC+6
 ,UJ,*ETAP3CC+1
 :6,ATX,MESADR1
 14,VJM,STOLONG
 GLOB***:6,XTA,MESADR1
 6,AAX,CNR40000
 7,UZA,
 6,AEX,MESADR1
 6,ATX,MESADR1
 6,XTA,GLOB*
 6,ARX,RCON1
 ,ASN,64-20
 6,AEX,UTCCONST
 6,ATX,UTCPR
 7,UJ,
 GLOB**:15,XTA,
 6,AEX,RCONY
 ,UZA,*ETAP3C1+4
 6,AAX,CNR7S12
 ,UZA,*ETAP3C1+3
 6,AEX,CNR7S12
 ,UZA,*ETAP3C1+3
 6,XTA,MESMOD
 6,AOX,BASIR
 ,UZA,*+3
 6,XTA,BLUTC1
 14,VTM,GLOB***
 ,UZA,STOLONG
 ,UJ,LONGER
 :6,XTA,MESADR1
 ,ASN,64+12
 15,ATX,
 6,AAX,RCON1
 6,ATX,-2
 6,AEX,NG1B
 6,STX,-1
 ,ASN,64+1
 6,ARX,-1
 6,ARX,GLOB*
 6,ATX,MESMOD
 6,XTA,-2
 ,UZA,*+2
 6,XTA,MESADR1
 6,AOX,CNR7S12
 ,UJ,SSILKCC
 :6,XTA,MESADR1
 6,AAX,CONR12
 6,ATX,MESADR1
 ,UJ,*ETAP3C0-1
 *CODES1:1,XTA,LOGPL-PER*3
 1,ATX,53
 :1,XTA,*C28-PER
 1,ATX,39
 :1,XTA,*C33-PER
 1,ATX,43
 1,XTA,*C34-PER
 1,ATX,52
 1,XTA,*REIHHH-PER
 1,ATX,42
 1,XTA,/D*/-PER
 1,ATX,38
 1,XTA,7
 1,ATX,28
 1,ATX,31
 1,XTA,*CM1-PER
 1,ATX,49
 13,UJ,
 TO41:6,XTA,CON4
 6,ATX,FLGADR
 6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 13,VJM,*WRI
 :6,ATX,IMIST
 6,ATX,RMIST
 6,ATX,MESADR1
 1,XTA,7
 1,ATX,11
 6,XTA,*EQPLG
 ,U1A,*+1
 13,VJM,*READ4
 ENDADR*:,XTA,
 6,ATX,IDFLAG
 6,ATX,IFFLAG
 ,UJ,ENDADR
 *ETAP3CB:6,ATX,MESADR1
 6,AAX,RCONY
 7,UZA,
 6,XTS,GLOB*
 ,U1A,GLOB**
 6,STX,RR2
 6,AEX,CNR7S12
 ,U1A,*+5
 *ETAP3CA:6,XTA,CN00001
 6,AEX,MESKOP
 6,ATX,MESKOP
 6,XTA,MESADR1
 6,AEX,CNR7S12
 13,UJ,
 :13,VTM,*ETAP3C0+1
 ,UJ,*ETAP3CA
 :6,ATX,RR3
 6,XTA,BASEMOD
 14,VTM,STOLONG+3
 ,UZA,*ETAP3C1
 6,XTA,XBASE
 6,AEX,MESMOD
 6,AOX,BASIR
  ,U1A,*ETAP3C1
 6,XTA,YSLUTC
 ,UZA,*+2
 6,XTA,BLUTC1
 ,UZA,*ETAP3C1
 :6,XTA,BASEMOD
 6,ATX,MESMOD
 6,XTA,MESADR1
 6,ARX,BASE*
 6,AAX,CONR15
 6,ATX,RR2
 6,ATX,MESADR1
 6,AAX,RCONY
 7,UZA,
 6,AEX,CNR7S12
 ,UZA,*ETAP3CA
 6,AEX,CNR4000
 ,U1A,*+4
 6,XTA,RR3
 6,AEX,CNR4000
 ,UZA,SSILKC4
 6,AEX,CNR7S12
 ,UZA,SSILKC31
 6,XTA,MESADR1
 ,UJ,SSILKCC
 :6,AEX,CNR7S12
 ,U1A,*+5
 6,XTA,RR3
 6,AEX,CNR4000
 ,UZA,SSILKC33
 6,AEX,CNR7S12
 ,UTC,
 ,UZA,SSILKC2
 :6,XTA,MESADR1
 ,UJ,SSILKC
 :,XTA,
 6,ATX,MESMOD
 *ETAP3C1:6,XTA,RR3
 6,AEX,CNR4000
 ,UTC,
 ,UZA,*+3
 6,AEX,CNR7S12
 ,U1A,*+3
 :6,XTA,MESADR3
 ,UJ,SSILKC
 :6,XTA,MESADR3
 ,UJ,SSILKCC
 :6,XTA,BLUTC1
 ,U1A,LONGER
 STOLONG:6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 6,XTA,UTCCONST
 6,ATX,UTCPR
 14,UJ,
 :6,XTA,MESADR3
 14,VTM,*ETAP3CC+6
 :6,ATX,MESADR1
 6,AAX,CNR40000
 ,UTC,
 7,UZA,
 :6,XTA,RMASK11
 6,ATX,MESADR2
 6,XTA,MESADR1
 6,ARX,MASK37
 *ETAP3CC:6,ATX,MESADR1
 6,ARX,M33S47
 :7,U1A,
 6,ARX,CONW
 ,U1A,*ETAP3CA+3
 6,XTA,MESADR2
 ,UZA,*ETAP3CC-2
 6,ARX,MESADR1
 6,ATX,MESADR1
 ,XTA,
 6,ATX,MESADR2
 14,UJ,
 :6,XTA,MESMOD
 6,ATX,-1
 6,XTA,RR2
 ,UZA,*+3
 :6,XTS,BASEMOD
 6,STX,MESMOD
 14,VTM,*+6
 ,UJ,*ETAP3CC
 :6,XTA,BASEMOD
 13,VTM,*ETAP3RL+6
 ,UZA,SSILKCL
 6,XTA,XBASE
 ,U1A,SSILKCL
 6,XTA,MESMOD
 6,AOX,BASIR
 13,VTM,*ETAP3C0+1
 ,U1A,*+1
 14,VTM,
 :6,XTA,MESADR1
 6,ARX,BASE*
 6,AAX,CONR15
 14,VZM,*-8
 6,XTS,UTCREG
 6,AEX,UTCPR
 6,STX,UTCPR
 14,VJM,*ETAP3CC
 :6,XTA,-1
 6,ATX,MESMOD
 6,XTA,UTCCONST
 6,ATX,UTCPR
 13,VTM,*ETAP3RL+6
 ,UJ,SSILKCL
 *ETAP3C2:6,XTA,BASPR
 ,UZA,*ETAP3C4-1
 6,XTA,MESADR3
 6,ATX,MESADR1
 6,XTA,UTCPR
 ,U1A,UTCPR*
 6,XTA,MESMOD
 6,AOX,BASIR
 13,VTM,*ETAP3RL+6
 ,UZA,*ETAP3C3
 6,XTA,BLDEF
 ,UZA,*ETAP3C4
 6,XTA,BLUTC1
 ,U1A,*ETAP3C4
 6,XTA,BASEMOD
 ,UZA,*ETAP3C4
 6,XTA,BASLONG
 ,UZA,*ETAP3C4
 6,XTA,XBASE
 6,AEX,YSLUTC
 ,UZA,*ETAP3C4
 6,XTA,UTCCONST
 6,ATX,UTCPR
 6,XTA,MESADR1
 6,AAX,CNR40000
 7,UZA,
 6,XTA,MESADR1
 6,AEX,CONR15
 :,UZA,*+4
 6,XTA,MESADR1
 :,ASN,64+1
 6,ATX,MESADR2
 ,ASN,64-1
 6,AEX,MESADR1
 6,ARX,MESADR2
 ,UJ,*ETAP3C0+1
 :6,XTA,XBASE
 ,U1A,*ETAP3C1-2
 6,XTA,UTCPR
 6,AEX,UTCREG
 6,ATX,UTCPR
 6,XTA,MESADR1
 6,ARX,BASE*
 6,AAX,CONR15
 6,ATX,MESADR1
 ,UJ,*-7
 *ETAP3C3:6,XTA,MESADR1
 6,AAX,CNR40000
 7,UZA,
 6,XTA,GLOB*
 ,UZA,*+3
 6,ARX,RCON1
 6,ATX,MESMOD
 6,XTA,MESADR1
 6,AEX,CNR40000
 ,UJ,*ETAP3C0+1
 :6,XTA,BASEMOD
 ,UZA,*ETAP3C1-2
 6,XTA,XBASE
 ,U1A,*ETAP3C1-2
 6,XTA,MESADR1
 6,ARX,BASE*
 6,ATX,-1
 6,AAX,CNR40000
 ,U1A,*ETAP3C1-2
 6,XTA,BASEMOD
 6,ATX,MESMOD
 6,XTA,-1
 6,AAX,CONR15
 ,UJ,*ETAP3C0+1
 UTCPR*:6,XTA,MESKOP
 6,AAX,CN00002
 ,U1A,*+2
 14,VJM,KOPNORM
 :14,VTM,SSILKCL
 ,UJ,*ETAP3CC
 :6,XTA,MESADR1
 6,AAX,CNR40000
 7,UZA,
 6,XTA,MESADR1
 6,AEX,CONR15
 ,UZA,*ETAP3C1-2
 6,XTA,MESADR1
 ,UJ,*ETAP3C2+15
 *ETAP3R:6,XTA,-1
 ,U1A,*ETAP3RL
 :6,ATX,-1
 6,ATX,-3
 13,VTM,*ETAP3C0+1
 14,VJM,KOPNORM
 :6,ATX,MESADR1
 6,XTA,GLOB*
 ,UZA,*ETAP3RR+11
 6,XTA,MESMOD
 6,AOX,BASIR
 ,UZA,*ETAP3RR+6
 6,XTA,BLUTC1
 ,U1A,*ETAP3RR+6
 6,ARX,XADRESS1
 14,VTM,SSILKCR
 ,UZA,STOLONG
 14,VJM,STOLONG
 :6,XTA,MESADR1
 ,UJ,*+3
 :6,XTA,RMASK11
 6,ATX,MESADR2
 6,XTA,MESADR3
 6,ARX,MASK37
 *ETAP3RR:6,AEX,CNR40000
 6,ATX,MESADR1
 6,AEX,CNR40000
 6,ARX,CONRL
 7,U1A,
 6,ARX,-1
 ,U1A,*ETAP3CA
 6,XTA,MESADR2
 ,UZA,*-6
 ,XTA,
 6,ATX,MESADR2
 ,UJ,SSILKCR
 :6,XTA,XADRESS1
 ,NTR,13B
 :,UZA,*+6
 6,XTA,MESADR1
 6,AAX,RCONY
 ,U1A,*+4
 6,XTA,MESKOP
 6,AEX,CN00001
 6,ATX,MESKOP
 7,UJ,
 :6,ARX,XADRESS1
 ,UZA,*+2
 :6,XTA,MESADR1
 ,UJ,SSILKR
 :6,XTA,MESADR1
 ,UJ,SSILKNR
 *ETAP3W3:6,XTA,RCON1
 6,ATX,BLBSS
 6,XTA,IDFLAG
 7,VTM,ENDADR*
 PLG10:,U1A,ENDAID-1
 ,UJ,PARAMX+3
 :6,AEX,CON1
 ,UTC,
 ,U1A,*+2
 6,XTA,MESADR3
 6,AEX,CON1
 6,ATX,MESADR4
 :6,XTA,MISTABL
 6,AAX,CN0002
 14,VTM,*+7
 ,U1A,PARNORM
 6,XTA,XADRESS1
 6,AAX,CON1
 13,VTM,PARAMX
 14,VTM,SSILKRD
 ,UZA,PARNORM
 6,AEX,XADRESS1
 14,VTM,SSILKR
 13,VTM,PARAMX+2
 ,UZA,PARNORM
 14,VTM,SSILKNR
 ,UJ,PARNORM
 :6,XTA,XADRESS1
 6,AAX,CON1
 13,VTM,ENDADR6+4
 7,VTM,ENDADR
 ,U1A,*ETAP3RL
 ,UJ,SSILKRD
 *ETAP3W2:6,XTS,MISTABL
 6,AAX,CN0002
 6,STX,-1
 6,AEX,BASYPR
 7,VTM,ENDADR
 13,VTM,ENDADR6+4
 7,MTJ,14
 ,UZA,*ETAP3Z1
 6,ATX,BASYPR
 6,ATX,BLBSS
 6,XTA,XADRESS1
 6,AEX,CON1
 :,U1A,*+2
 6,XTA,MESADR3
 6,AEX,CON1
 6,ATX,MESADR4
 :6,XTA,BASEMOD
 ,UZA,*+3
 6,XTA,XBASE
 ,U1A,*ETAP3W5
 6,XTA,YSLUTC
 ,U1A,BLDEFB*
 *ETAP3Z4:6,XTA,-1
 6,AOX,BLUTC1
 PLG11:14,VTM,ENDADR6
 14,U1A,
 6,XTA,GLOB*
 ,UZA,*ETAP3C0+1
 6,XTA,MESMOD
 6,AOX,BASIR
 :,UZA,*ETAP3C0+1
 ,UJ,STOLONG
 *ETAP3Z2:6,XTA,XADRESS2
 ,U1A,*ETAP3Z3
 :6,XTA,XADRESS1
 6,AAX,CON2
 ,UZA,*ETAP3R
 6,XTA,-1
 ,U1A,SSILKRD
 14,VJM,KOPNORM
 :6,XTA,GLOB*
 13,VTM,ENDADR-2
 ,UZA,SSILKRD
 6,XTA,BLUTC1
 ,U1A,SSILKRD
 6,XTA,MESMOD
 6,AOX,BASIR
 14,VTM,*ETAP3Z5+2
 ,UZA,SSILKRD
 ,UJ,STOLONG
 *ETAP3Z3:6,XTA,-1
 14,VJM,ENDADR-5
 :8,VJM,ADR*RA
 :13,VJM,SSILKSD
 :6,ATX,MESADR1
 ,UJ,*ETAP3Z2+4
 *ETAP3Z1:6,XTA,UTCPR
 ,UZA,*ETAP3Z4
 :6,XTA,MESKOP
 6,AAX,CN00002
 ,U1A,*ETAP3Z5+6
 14,VJM,KOPNORM
 :6,XTA,XADRESS2
 8,VTM,*ETAP3Z5
 14,VTM,*ETAP3Z5+2
 ,U1A,ADR*RA
 :6,XTA,XADRESS1
 6,AAX,CON1
 13,VTM,*ETAP3C0+1
 ,UZA,*ETAP3Z5+2
 6,ATX,-3
 6,ATX,-1
 6,XTA,GLOB*
 ,U1A,*+3
 6,XTA,CONRL
 6,ATX,-1
 6,XTA,CNR7S12
 6,ATX,-3
 :6,ARX,XADRESS1
 ,UZA,SSILKCR
 6,XTA,MESADR3
 ,UJ,*ETAP3RR
 *ETAP3Z5:13,VJM,SSILKSD
 :6,ATX,MESADR1
 14,UJ,
 :6,XTA,MESADR3
 6,AAX,RMASK11
 :6,ATX,MESADR2
 6,AEX,MESADR3
 6,ATX,MESADR3
 13,VTM,ENDADR6+4
 7,VTM,ENDADR
 ,UJ,SSILKRD
 :6,XTA,XADRESS2
 8,VTM,*ETAP3Z5
 14,VTM,*+1
 ,U1A,ADR*RA
 :6,XTA,XADRESS1
 6,AAX,CON2
 ,UZA,*+2
 6,XTA,MESADR3
 6,AAX,RMASK14
 ,UJ,*ETAP3Z5+3
 :6,XTA,MESADR3
 6,AAX,RMASK14
 6,ATX,MESADR2
 6,AEX,MESADR3
 6,ATX,MESADR3
 ,UJ,*ETAP3RL
 BLDEFB*:6,XTA,MESMOD
 6,AOX,BASIR
 ,UZA,*ETAP3Z4
 6,XTA,BLUTC1
 ,U1A,*ETAP3Z4
 6,XTA,XADRESS1
 6,AEX,CON1
 6,AOX,XADRESS2
 6,ARX,NGZB
 6,AAX,RCON1
 6,AOX,BLDEF
 6,AOX,BASYSL
 ,UZA,*ETAP3Z4
 6,XTA,-1
 PLG12:14,VTM,ENDADR6
 ,UZA,STOLONG
 PLG13:6,XTA,BASLONG
 ,UJ,*ETAP3Z2-1
 :6,XTA,BASLONG
 ,UZA,*ETAP3Z2
 14,VTM,*ETAP3Z5+6
 ,UJ,STOLONG
 *ETAP3W5:6,XTA,-1
 ,UZA,*+5
 6,XTA,BASLONG
 ,U1A,*+4
 PLG14:6,ATX,MESADR1
 7,UJ,
 ,U1A,*ETAP3RL
 6,XTA,MESADR2
 ,UZA,SSILKRD
 ,UJ,ENDADR-5
 :6,XTA,MESMOD
 6,AOX,BASIR
 ,UZA,*ETAP3W6
 6,XTA,BLUTC1
 ,U1A,*ETAP3Z4
 8,VJM,BTEST
 PLG16:,U1A,PLG15+3
 6,ATX,XADRESS1
 6,ATX,MESADR1
 14,VJM,STOLONG
 :6,XTA,BLDEF
 6,AOX,YSLUTC
 PLG15:6,ATX,BLBSS
 7,UJ,
 :6,ATX,MESADR1
 ,XTA,
 6,ATX,XADRESS5
 ,UJ,UTCPR*
 :6,XTA,XADRESS1
 6,AEX,CON1
 6,AOX,XADRESS2
 14,VTM,ENDADR6
 ,UTC,
 ,UZA,STOLONG
 :6,XTA,BASYSL
 ,UJ,*+6
 :6,XTA,XADRESS1
 6,AEX,CON1
 6,AOX,XADRESS2
 14,VTM,BASFORM
 ,UTC,
 ,UZA,STOLONG
 :6,XTA,BASYSL
 ,U1A,STOLONG
 6,XTA,BLDEF
 14,VTM,*ETAP3Z1+1
 :,UZA,*ETAP3Z4
 ,UJ,STOLONG
 *ETAP3W6:6,XTA,YSLUTC
 6,AOX,BLDEF
 15,ATX,
 6,AOX,BLUTC1
 6,ATX,BLUTC1
 8,VTM,
 BTEST:6,XTA,XADRESS2
 ,UZA,*+4
 6,XTA,BASE**
 14,VTM,*+2
 ,U1A,IFFORM
 6,XTA,XBASE
 :6,AEX,BASE**
 ,UJ,*+3
 :6,XTA,XADRESS1
 6,AEX,MESADR1
 6,AEX,CON4
 6,AEX,XBASE
 :8,UTC,
 8,V1M,
 ,U1A,PLG18
 6,STX,XADRESS1
 6,ATX,BLBSS
 14,VJM,BASFORM
 :6,ATX,MESADR1
 ,XTA,
 PLG17:6,ATX,XADRESS5
 6,ATX,MESADR1
 6,XTA,-1
 6,AEX,BLUTC1
 7,U1A,
 ,UJ,PLG9+4
 6,XTA,-1
 14,VTM,*ETAP3CB
 13,VTM,*ETAP3C0+1
 ,UZA,KOPNORM
 ,UJ,*ETAP3C4
 PLG18:15,UTM,-1
 ,UJ,ENDADR6
 6,AEX,CON1
 6,AOX,XADRESS2
 ,UZA,BASFORM
 6,XTA,BASYSL
 ,UZA,*ETAP3Z2
 ,UJ,BASFORM
 :6,AEX,UTCREG
 6,ATX,UTCPR
 ,UJ,BASFORM+2
 BASFL0:6,XTA,MESADR5
 14,UJ,
 :6,XTA,XADRESS2
 ,U1A,*+2
 6,XTA,MESADR1
 ,UJ,BASFL
 :6,XTA,XBASE
 8,ATX,2
 ,XTA,
 ,UJ,BASFL1
 BASFORM:6,XTA,UTCPR
 ,U1A,BASFL0-2
 :6,XTA,BASEMOD
 6,ATX,MESMOD
 :6,XTA,XADRESS5
 6,ATX,XADRESSB
 6,XTA,MESADR5
 6,ATX,MESADRB
 6,XTA,BASE*
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 6,ATX,MESADR5
 6,XTA,XADRESS1
 ,UZA,BASFL0
 15,UTC,1
 8,VTM,
 6,AEX,MESADR1
 8,ATX,
 6,XTA,XADRESS2
 6,AEX,MESADR2
 8,ATX,1
 6,XTA,RCON1
 6,ATX,BLUTC1
 6,XTA,BASE**
 ,UZA,BASFL0+1
 6,AEX,CON4
 6,AAX,CONBAS
 8,ATX,2
 6,XTA,BASE**
 ,ASN,64-3
 6,AAX,CON7
 8,ATX,3
 6,XTA,BASE**
 ,ASN,64+12
 6,AAX,CONR12
 8,AEX,3
 8,AEX,2
 BASFL1:8,ATX,3
 ,ITA,
 8,ATX,4
 7,VJM,*ETAP31-1
 :6,XTA,XADRESS2
 7,VTM,ENDADR
 ,UZA,*+3
 8,VJM,ADR*R
 :13,VJM,SSILKSD
 BASFL:6,ATX,MESADR1
 6,XTA,XBASE
 6,AAX,CON7
 6,ATX,XADRESS2
 6,AEX,XBASE
 6,ATX,MESADR2
 6,XTA,CON2
 6,ATX,XADRESS5
 6,XTA,UTCPR
 ,U1A,*ETAP3Z1+1
 6,XTA,MISTABL
 6,AAX,CN0002
 14,VTM,ADR*RA
 8,VTM,*+3
 :,UZA,KOPNORM
 8,VJM,ADR*RA
 13,VTM,ENDADR6+3
 ,UJ,SSILKSD
 :13,VTM,ADR1*0
 8,VJM,SSILKSD
 :6,ATX,MESADR1
 6,ARX,XADRESS1
 13,VTM,PARAMX+3
 ,UZA,ENDADR6+6
 ENDADR:8,VTM,
 13,VJM,*CODES1
  :1,XTA,7
 1,ATX,10
 1,AEX,11
 ,U1A,TO41
 15,WTC,
 13,VTM,
 6,ATX,BASPR
 13,UJ,
C
C**************************************
C*
C*  ПOИCK ИДEHTИФИKATOPA B TAБЛИЦE.
C*
C
 POISKI:,XTA,
 6,XTS,RR1
 :6,ATX,A8
 12,VTM,-3
 :6,AAX,AR
 6,XTS,A8
 ,ASN,64-11
 6,ATX,A8
 15,ARX,
 12,VLM,*-2
 ,ASN,64-12
 ,YTA,
 12,VTM,SECTOR
 ,ITS,12
 15,ARX,
 6,ATX,AH
 ,STI,12
 ,U1A,*POL
 *POI    : 12,  XTA  ,1
            6,  AEX  ,CONSS
 ,UZA,ISKOUT0
 12,XTA,
 6,AEX,RR1
 ,UZA,ISKOUTE+2
 :12,UTM,2
 ,ITA,12
 6,AEX,AB
 ,UZA,ISKOUTE+1
 ,ITA,12
 6,AEX,AH
 ,UTC,
 ,U1A,*POI
 ISKOUTE:6,XTA,RCON1
 14,UJ,
 :12,VTM,SECTOR
 ,UJ,*POI
 :12,XTA,1
 6,AAX,CON7
 14,UZA,
 ,UJ,*POI+3
 ISKOUT0:,ITA,14
 14,UJ,
C
C**************************************
C*
C*  ПOИCK KOHCTAHTЫ LITERAL B TAБЛИЦE.
C*
C
 POISKL:6,XTA,CON4
 6,XTS,RR1
 ,UJ,POISKI+1
 *POL    : 12,  XTA  ,1
            6,  AEX  ,CONSS
 ,UZA,ISKOUT0
 12,XTA,
 6,AEX,RR1
 ,UZA,*+6
 :12,UTM,2
 ,ITA,12
 6,AEX,AB
 ,UZA,*+3
 ,ITA,12
 6,AEX,AH
 ,U1A,*POL
 ,UJ,ISKOUTE
 :12,VTM,SECTOR
 ,UJ,*POL
 :12,XTA,1
 6,AEX,RR2
 6,AAX,MASK24
 14,UZA,
 ,UJ,*POL+3
C
C**************************************
C*
C*  OБPAБOTKA  MHEMOKOДA.
C*
C
 :6,WTC,PSKPLG
 ,UJ,
 POSKOP:6,XTA,PSKPLG
 ,U1A,*-1
 :6,XTA,BLDEF
 14,VTM,
 ,U1A,*POSKOP4
 6,XTA,MESKOP
 ,UZA,RE8KOP
 6,AAX,CUTH
 ,ASN,64-8
 6,AEX,PACKH
 ,NTR,2
 6,A*X,HASH
 6,AAX,RCON377
 ,ASN,64-1
 6,XTS,MESKOP
 ,NTR,7
 15,WTC,
 14,VTM,KOPTAB
 :14,AEX,
 14,UZA,1
 14,WTC,1
 14,VTM,
 6,XTA,MESKOP
 14,V1M,*-2
 ,ASN,64+6
 ,UZA,*POSKOP4
 ,ASN,64+6
 ,U1A,POSKOP3
 6,XTA,MESKOP
 13,VJM,PER6T8
 :15,ATX,
 ,ASN,64-15
 6,STX,MESKOP
 ,ASN,64+4
 ,UZA,*POSKOP4
 ,ASN,64+1
 6,AOX,RMIST
 ,UZA,*RELON
 *POSKOP4:6,XTA,CON04
 6,AOX,MISTABL
 6,AAX,NCON3
 6,ATX,MISTABL
 ,XTA,
 6,ATX,IMIST
 6,ATX,RMIST
 ,UJ,*ABP1A+1
 POSKOP3:,ASN,64+6
 ,U1A,POSKOPZ
 6,XTA,MESKOP
 13,VJM,PER6T8
 :15,ATX,
 ,ASN,64-12
 6,STX,MESKOP
 ,ASN,64+7
 6,AOX,RMIST
 ,UZA,RE8KOP
 ,UJ,*POSKOP4
 POSKOPZ:,ATI,14
 ,ASN,64+6
 ,U1A,*POSKOP4
 ,ITA,14
 ,ASN,64-18
 6,AEX,MESKOP
 14,UTM,-72B
 14,V1M,*POSKOP4
 13,VTM,R*ZL-5
 ,UJ,PER6T8
C
C**************************************
C*
C*  OБPAБOTKA ПOЗИЦИИ METKИ.
C*
C
 POSMET:6,XTA,IMIST
 6,XTS,INDTABL
 6,ATX,ASTNUMET
 ,UZA,*+2
 13,VTM,LOI
 12,VJM,*WRIN
 :6,STX,MESMET
            6,  XTA  ,MISTABL
            6,  AOX  ,CN0004              ПPИЗHAK ПOMEЧEHHOЙ ИHCTPYKЦИИ
            6,  ATX  ,MISTABL
 1,XTA,7
 1,ATX,28
            6,  XTA  ,IMIST
 ,UZA,ABP
            6,  XTA  ,MISTABL
            6,  AOX  ,CON6
 15,UTM,1
 6,ATX,ASTMET
            6,  ATX  ,MISTABL
             ,  XTA  ,
 6,ATX,NUMMET
 6,STX,IMIST
 ,U1A,ABP
 6,XTA,POSIND
 6,AEX,FLAG3B
 6,ATX,ASTMET
 ,UZA,ABP
 6,XTA,POSIND
 6,ATX,NUMMET
 ,UJ,ABP
C
C**************************************
C*
C*  OБPAБOTKA ПOЗИЦИИ MOДИФИKATOPA.
C*
C
 POSMOD:6,XTA,INDTABL
 6,ATX,IRB
 13,VTM,*POSMOD1
 ,U1A,*WRIN
 :6,XTA,IMIST
 ,U1A,*POSMOD2
 :1,XTA,/2/-PER
 1,ATX,13
 1,XTA,/3/-PER
 1,ATX,15
 1,XTA,/5/-PER
 1,ATX,14
 1,XTA,7
 1,ATX,28
 ,UJ,ABP
 *POSMOD1:6,XTA,POSIND
 6,ATX,MESMOD
 6,AEX,CON2
 ,ASN,64+1
 6,ARX,CON33
 ,U1A,POSMOD+2
 6,XTA,MISTABL
 6,AOX,CN0001
 6,ATX,MISTABL
 6,XTA,POSIND
 6,AEX,FLAG3B
 ,U1A,POSMOD+2
 *POSMOD2:6,XTA,MISTABL
 6,AOX,CON1
 6,ATX,MISTABL
 ,XTA,
 6,ATX,IMIST
 ,UJ,POSMOD+3
C
C**************************************
C*
C*  CTAHДAPTHAЯ ПPOЦEДYPA BЫДAЧИ
C*  OЧEPEДHOГO CИMBOЛA KAPTЫ.
C*
C
 *READ5:6,XTA,LONGSTR
 6,ATX,NN
 ,ITA,6
 ,UJ,*READ4+1
 :6,WTC,AB*PLG
 ,UJ,
 READA:6,XTA,AB*PLG
 ,U1A,*-1
 :6,XTA,NSTR+2
 4,VLM,*READ1
 *READ3:,ITA,4
 6,ARX,NSTR+1
 6,AAX,CONBAS
 6,ATX,NN
 *READ4:,ITA,6
 ,UTC,
 :6,ATX,NSTR
 6,XTA,R3B
 6,ATX,NSTR+1
 ,XTA,
 6,ATX,NSTR+2
 4,VTM,-42
 14,VTM,177B
 13,UJ,
 :6,WTC,NSTR
 14,VTM,1
 ,ITA,14
 6,ATX,NSTR
 14,XTA,-1
 ,UTC,
 *READ1:,UZA,*-3
 ,ASN,64-8
 6,ATX,NSTR+2
 ,YTA,
 ,ATI,14
 13,UJ,
 :6,XTA,AB*PLG
 ,U1A,READA
 READ:6,XTA,NSTR+2
 4,VLM,*READ12
 ,UJ,*READ3
 :6,WTC,NSTR
 14,VTM,1
 ,ITA,14
 6,ATX,NSTR
 14,XTA,-1
 *READ12:,UZA,*-3
 ,ASN,64-8
 6,ATX,NSTR+2
 ,YTA,
 ,ATI,14
 14,UTM,-MACSYV
 14,VZM,READ
 14,UTM,-MACSYV1+MACSYV
 14,VZM,READ
 14,UTM,MACSYV1
 LSETL1:13,UJ,
 ,UJ,LSETL+2
C
C**************************************
C*
C*  ЛOKAЛЬHOE БAЗИPOBAHИE. OБPAБOTKA
C*  ИHCTPYKЦИЙ BASE, BAS, REL И RELS.
C*
C
 R*BAS:6,XTA,CON4
 13,VJM,R*BASE+1
 R*REL:6,XTA,CON2
 13,VJM,R*BASE+1
 R*RELS:6,XTA,CON02
 13,VJM,R*BASE+1
 :6,AAX,CON01
 13,VTM,BAS2
 ,U1A,FLAG2T-2
 ,UJ,*MACAB
 R*BASE:,XTA,
 13,VTM,
 :6,ATX,IFCNT
 6,XTA,GLOB*
 ,U1A,*VMET6
 6,ATX,BLUTC
 6,ATX,UTCREG
 6,ATX,BASLONG
 6,ATX,BASYSL
 6,ATX,BASEMOD
 6,ATX,MESADR1
 6,ATX,YSLUTC
 6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 6,XTA,FLAGTABL
 13,VZM,R*RELS+1
 6,AAX,CON1
 ,U1A,*MACAB
 13,VJM,FLAG2T
 BAS2:6,XTA,MESMOD
 ,U1A,*+3
 6,XTA,IFCNT
 13,VTM,BASE*COM
 ,U1A,*RENS99
 ,UJ,PADRESS
 1,XTA,*-PER*1
 6,ATX,*KOPPLG
 :,UJ,*RE8KOM
 ,UJ,*+1
 :1,XTA,TREP*C1-PER
 1,ATX,14
 6,ATX,*EQPLG
 13,VJM,PADRESSC
 :6,ATX,*EQPLG
 6,XTA,TSCF
 6,AEX,CON4
 13,VTM,*+3
 ,U1A,*READ3
 1,XTA,7
 13,VTM,*+2
 ,UJ,*RENA+1
 :,XTA,
 ,UJ,*+3
 :6,XTA,INDTABL
 6,AOX,IMIST
 6,ATX,BLUTC
 13,VJM,*WRI
 :6,ATX,IMIST
 6,XTA,BLDEF
 6,ATX,YSLUTC
 6,XTA,MESADR1
 ,UZA,*+1
 6,AEX,CON4
 :6,ATX,XBASE
 6,AEX,CON4
 6,AAX,CON4
 6,ARX,NGZB
 6,AAX,XBASE
 ,ASN,64-36
 6,ATX,VERET
 6,XTA,MESADR3
 6,ARX,CONR15
 6,AEX,CONR15
 6,AAX,CONR15
 6,ATX,BASE*
 6,AOX,XBASE
 ,UZA,BASE*COM+4
 :6,XTA,MESMOD
 ,UZA,BASE*COM+2
 6,ATX,BASEMOD
 ,ASN,64-20
 6,ATX,UTCREG
 6,XTA,IFCNT
 ,U1A,BAS9
 7,VJM,ENDADR6
 BASE*COM:6,XTA,BASKOP
 6,ATX,MESKOP
 ,UJ,*RE8KOP8+1
 :6,XTA,IFCNT
 7,VTM,BASE*COM
 ,U1A,BAS8
 ,UJ,ENDADR6
 :6,XTA,MESMOD
 ,UZA,BAS9+3
 6,XTA,BLDEF
 ,UZA,BAS9+3
 6,XTA,BLUTC
 ,U1A,BAS9+3
 PLG19:,UJ,*+2
 ,UTC,
 6,ATX,MISTABL
 13,VJM,FATERR+4
 :6,XTA,MESMOD
 6,ATX,BASEMOD
 ,ASN,64-20
 6,ATX,UTCREG
 6,XTA,IFCNT
 ,UZA,BASE*COM
 BAS9:,ASN,64-1
 ,UZA,BAS8
 6,ATX,BASLONG
 ,ASN,64-1
 ,UZA,BAS8
 6,ATX,BASYSL
 :6,XTA,IFCNT
 ,UZA,BASE*COM
 BAS8:13,VTM,ABP1
 ,UJ,DECPRINT
C
C**************************************
C*
C*  OБPAБOTKA  BSS  И  ENTRY  .
C*
C
 R*ENTRY:6,XTA,FLAGTABL
 6,AAX,CON104
 ,U1A,*MACAB
 13,VJM,LEFCOM
 13,VJM,*READ3
 6,XTA,MISTABL
 6,AAX,CON2
 ,U1A,*REBSS2
 6,XTA,NK
 6,ARX,KBSSAD
 13,VTM,*BSSCOM
 6,XTS,MESMET
 7,VJM,ISKENTRY
 :15,XTA,
 6,ATX,NK
 ,UJ,ABP1
 R*BSS:6,XTA,FLAGTABL
 6,AAX,CON1
 ,U1A,*MACAB
 13,VJM,LEFCOM-1
 :6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 13,VJM,PADRESSF
 :6,XTA,BLBSS
 6,AOX,BLDEF
 ,U1A,NOABSDEF
 6,ATX,MESADR4
 RJR:6,XTA,FLAGTABL
 6,AAX,CON004
 ,U1A,*REBSS10
 6,XTA,NK
 6,AEX,NKBSS
 7,VTM,*REBSS7
 ,UZA,*BSSCOM
 13,VJM,BSSPRINT
 6,XTA,MESADR3
 ,UZA,*ABP1
 6,ARX,BSSCNT
 6,ATX,BSSCNT
 :6,XTA,MESADR3
 6,AEX,CONR15
 ,ATI,7
 7,UTM,1
 13,VTM,*
 7,VLM,*MACB1
 T*RE8:,UJ,*ABP1
 ,UJ,*RE8KOP8
 *REBSS7:6,XTA,KBSSAD
 6,ARX,MESADR3
 6,ATX,BSSCNT
 6,STX,KBSSAD
 6,ATX,NK
 ,UJ,*ABP1
 *REBSS10:6,XTA,MESADR3
 ,UZA,*REBSS2
 6,ARX,DATCNT
 6,ATX,DATCNT
 13,VTM,*REBSS7-4
 ,UJ,BSSPRINT
 *REBSS2:13,VTM,*ABP1
 ,UJ,BSSPRINT
 *BSSCOM:6,XTA,NK
 15,ATX,
 6,ARX,KBSSAD
 7,MTJ,13
 6,ATX,NK
 ,UJ,BSSPRINT
 NOABSDEF:6,XTA,BLDEF
 13,VTM,*+2
 :,U1A,NODEFAD
 ,UJ,NOABSAD
 :,ITA,
 6,ATX,MESADR3
 6,ATX,MESADR4
 ,UJ,RJR
C
C+++++++++++++++++++++++++++++++++++++++
C+
C+   OБPAБOTKA ИHCTPYKЦИИ  CALL
C+
C
 R*CALL:6,XTA,FLAGTABL
 6,AAX,NCON4
 6,ATX,FLAGTABL
            6,  AAX  ,CON01
 ,UZA,*RECALL2
           13,  VJM  ,IDADR               AДPEC
         :  6,  XTA  ,MISTABL
            6,  AAX  ,CON02
 ,U1A,BAS8
 6,XTA,CONSUB
            6,  XTS  ,MESADR1
           13,  VJM  ,TOTIN               ЗAHECEHИE B TИ
 6,XTA,FLAGTABL
 6,AOX,CN0004
 6,ATX,FLAGTABL
 13,VJM,FLAG2T-2
         :  6,  XTA  ,MESADR1
           13,  VJM  ,ISKADR              AДPEC
 :15,XTA,-1
 6,AAX,CON7
 6,ATX,XADRESS1
 6,ATX,XADRESS5
 15,AEX,
 6,ATX,MESADR1
 6,XTA,CN0002
 6,ATX,-1
 6,AOX,MISTABL
 6,ATX,MISTABL
 6,XTA,MESMOD
 ,UZA,*RECLL1+1
 1,XTA,*RECLL1-PER
 6,ATX,*KOPPLG
 *RECLL1:,UJ,*RE8KOM
 ,UJ,*+2
 :6,XTA,RCON15
 6,ATX,MESMOD
 :6,XTA,CON*31
 6,ATX,MESKOP
 1,XTA,T*RE8-PER
 ,XTS,
 6,ATX,BLUTC1
 6,ATX,XADRESS2
 6,ATX,MESADR2
 6,ATX,MESADR3
 6,ATX,MESADR4
 6,ATX,XADRESSB
 6,ATX,MESADRB
 6,XTA,CON1
 6,ATX,BASIR
 6,ATX,BASPR
 ,UJ,*ETAP3W2+2
 *RECALL2:13,VTM,*MACAB
             ,  UJ   ,MET
C
C**************************************
C*
C*  OБPAБOTKA  DATA  И  SET  .
C*
C
 R*DATA:6,XTA,FLAGTABL
 6,AAX,CON1
 ,U1A,*MACAB
 13,VJM,LEFCOM
 6,XTA,FLAGTABL
 6,AAX,NCON01
 6,AOX,CON004
 6,ATX,FLAGTABL
 6,ATX,SIGN
 6,AAX,RCON1
 13,VTM,*+4
 ,U1A,*READ3
 6,XTS,BSSCNT
 6,X-A,NK
 6,STX,NK
 6,ATX,BSSCNT
 13,VTM,*ABP1
 ,UJ,*READ4
 :6,XTA,NK
 6,AEX,NKKOS
 13,VTM,*+1
 ,UZA,LITWRITE
 :13,VTM,*ABP1
 ,UJ,BSSPRINT
 R*SET:6,XTA,FLAGTABL
 6,AAX,NCON01
 6,AOX,CON1
 6,ATX,FLAGTABL
 1,XTA,*RESETC-PER
 6,ATX,PSKPLG
 6,XTA,SETCNT
 6,ARX,RCON1
 6,ATX,SETCNT
 13,VJM,LEFCOM
 ,ITA,
 6,ATX,IFNAM
 :1,XTA,LSETL-PER
 6,XTS,MESMOD
 ,UZA,*+3
 1,XTA,*REDA23-PER
 REDAT:6,ATX,*KOPPLG
 6,XTA,MASK33
 6,ATX,MODMASK
 ,UJ,*RE8KOM
 :6,XTA,RCON1
 6,ATX,MESMOD
 REDA1:,ITA,15
 6,ATX,IFCNT
 1,XTA,*REDA43-PER
 1,ATX,14
 1,XTA,TSC0-PER
 1,ATX,10
 *REDA44:13,Z31,PADRESSI
 ,UJ,REDAT1+7
 :6,XTA,MESMOD
 6,AAX,MASK37
 ,U1A,*REDA23+1
 6,XTA,MESMOD
 :,ASN,64-12
 6,ATX,MESMOD
 :6,XTA,MESADR3
 ,UZA,*+2
 6,ATX,MESADR1
 6,AAX,MASK37
 :,U1A,REDRED
 6,XTA,MESADR1
 :6,AEX,MESMOD
 13,VTM,*FLAG2T
 *REDA23:,UJ,COMMAND
 ,UJ,REDA1
 :6,XTA,MESMOD
 13,VTM,*REDA44+3
 *RESETC:,UJ,SSILKC
 ,UJ,*+1
 :,XTA,
 6,ATX,PSKPLG
 6,XTA,MISTABL
 6,AAX,CON04
 6,AOX,MESKOP
 13,VTM,REDATE
 ,U1A,NOENDOP
 1,XTA,LSETL1-PER
 13,XTS,*REDA24-REDATE
 ,UJ,REDAT
 REDRED:6,XTA,MESADR1
 13,VTM,*REDA23-1
 LSETL:,UJ,SSILKC
 ,UJ,*+1
 :6,XTA,RCON1
 6,ATX,IFNAM
 :6,XTA,LEFINST
 13,VTM,*ABP1
 ,UJ,SETPRI
 REDATE:,XTA,
 13,VTM,POSKOP
 *REDA33:,UJ,COMMAND
 ,UJ,*TEXEND1-2
 REDAT1:1,XTA,*REDA44-PER
 6,WTC,IFCNT
 ,ATX,-1
 6,ATX,*EQPLG
 ,XTA,
 6,ATX,PSKPLG
 6,XTA,TSCF
 ,UZA,*+3
 1,XTA,7
 1,ATX,14
 1,WTC,11
 ,UJ,
 :1,WTC,97
 ,UJ,
 :6,ATX,*EQPLG
 1,XTA,7
 1,AEX,14
 ,UZA,*REDA43+1
 1,XTA,*REDA45-PER
 1,ATX,14
 :1,XTA,*REDA33-PER
 6,XTS,CNR10000
 6,ATX,MESMOD
 13,VTM,*REDA23-4
 *REDA43:,UJ,PADRESSI
 ,UJ,REDAT1
 :1,XTA,*REDA45-PER
 1,ATX,11
 *REDA45:,UJ,*REDA43-2
 ,UJ,*+1
 :1,XTA,*REDA46-PER
 6,WTC,IFCNT
 ,ATX,
 1,XTA,7
 1,ATX,11
 1,ATX,14
 6,ATX,*EQPLG
 ,UJ,REDAT1+6
 :6,XTA,MESADR1
 6,XTS,MESADR3
 ,XTS,
 6,ATX,*EQPLG
 PL*IFM:13,Z31,PADRESSI
 ,UJ,REELSE*
 :6,XTA,MESADR3
 13,VTM,*REDA23-4
 ,U1A,*REDA46+1
 6,XTA,MESADR1
 ,U1A,*REDA24+1
 15,XTA,
 6,STX,MESADR3
 6,ATX,MESADR1
 *REDA46:13,UJ,
 ,UJ,*REDA45+5
 *REDA24:,UTC,1
 13,VTM,*REDA44
 :6,STX,MESMOD
 ,UJ,*REDA46-1
C
C**************************************
C*
C*  OБPAБOTKA  INT  И  REAL  .
C*
C
 R*INT:,UTC,EQI-EQR
 R*REAL:7,VTM,EQR
 13,VJM,*MADEC1
 PLG22:13,VTM,FATERRD+2
 ,UJ,*MADEC2
 15,ATX,
 8,VTM,1
 REALRET:7,UJ,
 ,UJ,*+1
 :,XTS,
 6,STX,CODPLG
 13,VTM,*TEXEND1-2
 ,UJ,*MADEC2
C
C**************************************
C*
C*  OБPAБOTKA KOMAHД.
C*
C
 :15,ATX,
 ,ASN,64-12
 6,STX,MESKOP
 ,ASN,64+7
 15,ATX,1
 ,ASN,64+1
 6,AOX,RMIST
 ,U1A,*POSKOP4
 15,XTA,1
 ,UZA,R*Z
 R*ZL:6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 ,UJ,R*Z
 RE8KOP:6,XTA,CON1
 6,ATX,BASPR
 6,XTA,MESMOD
 6,ATX,BASIR
 6,ARX,NGZB
 6,AAX,MASK*20
 6,AEX,BASIR
 6,ATX,BLUTC1
 6,ARX,NGZB
 6,AEX,BLUTC1
 6,AOX,BLUTC
 6,ATX,BLUTC1
 R*Z:6,XTA,FLAGTABL
 6,AAX,CON01
 13,VTM,*RE8KOM
 ,UZA,*RECALL2
 14,XTA,1
 14,VZM,FLAG2T-2
 6,AAX,CONKP1
 ,ASN,64+3
 6,ATX,MESKOP
 ,UJ,FLAG2T-2
 *RE8KOM:6,XTA,MISTABL
 6,AAX,CONRE81
 12,VTM,
 ,UZA,*RE8KOP6-5
 6,AAX,CON1
 ,U1A,*R8KOP6A-1
            6,  XTA  ,MESMOD
 13 ,VJM,ISKADR
 :6,ATX,RR1
 ,ITA,
 6,STX,BLDEF
 ,U1A,NOABSID
 6,XTA,RR1
 ,UJ,*RE8KOP6+1
 NOABSID:,ITA,
 6,ATX,RR2
 6,XTA,MESMOD
 12,VZM,*R8KOP6A-1
 13,VTM,*R8KOP6A-1
 ,UJ,NOABSI
 :6,ATX,RR2
 6,XTA,IRB
 ,UZA,*RE8KOP6+3
 6,AEX,BSIMB
 6,AAX,AA6SIMBR
 ,U1A,*RE8KOP6
 6,XTA,IRB
 ,ASN,64+6
 13,VTM,*RE8KOP6+1
 ,UJ,PER6T8
 *RE8KOP6:  6,  XTA  ,MESMOD
 13,VJM,PERI10
            6,  ATX  ,MESMOD
 6,AAX,MODMASK
 6,AOX,RMIST
 ,U1A,LONGMOD
 :6,XTA,MISTABL
 6,AAX,CN000001
 6,AEX,CN000001
 6,AEX,MESMOD
 ,UZA,ZEROMOD
 6,XTA,*KOPPLG
 :13,VTM,*RE8KOP8
 ,UZA,PADRESS
 ,ATI,13
 6,XTA,MASK44
 6,ATX,MODMASK
 ,XTA,
 6,ATX,*KOPPLG
 13,UJ,
 *RE8KOP8:6,XTA,UTCPR
 ,U1A,*R8KOP6A+4
 :6,XTA,MESMOD
 ,ASN,64-20
 6,AEX,MESKOP
 6,AEX,MESADR1
 PLGKOP:15,ATX,
 ,UTC,
 :,XTA,
 6,ATX,UTCPR
 6,XTA,*COMPLG
 8,VTM,
 13,VTM,ABP1
 ,U1A,*+2
 15,XTA,
 ,UJ,COMMAND
 :,ATI,13
 ,XTA,
 6,STX,*COMPLG
 ,UJ,COMMAND
 :,UTC,CON4-CON1
 *R8KOP6A:6,XTA,CON1
 6,AOX,MISTABL
 6,ATX,MISTABL
 ,XTA,
 :6,ATX,RMIST
 6,ATX,MESMOD
 6,XTA,*KOPPLG
 ,UJ,*RE8KOP8-4
 :6,AEX,MESADR1
 6,XTS,MESADR2
 6,ATX,MESADR1
 6,ARX,CONR15
 6,AEX,CONR15
 6,ARX,MESADR5
 6,AAX,CONR15
 6,ATX,MESADR5
 PLGKOP1:,UTC,
 ,UTC,
 :15,XTA,
 8,VTM,1
 13,VTM,*RE8KOP8+1
 ,UJ,COMMAND
 LONGMOD:6,XTA,RMIST
 13,VTM,*R8KOP6A-1
 ,U1A,*R8KOP6A
 ,UJ,LONGM
 ZEROMOD:13,VTM,*R8KOP6A-1
 ,UJ,ZEROM
C
C**************************************
C*
C*  OБPAБOTKA ATPИБYTOB.
C*  (ДЛИHHOAДPECHAЯ KOMAHДA, ABTOMATИ-
C*  ЧECKAЯ ПOMEЧEHHOCTЬ И T. П.)
C*
C
 RELONG  :   ,  BSS  ,                    BXOДЫ
 :,UJ,RE8KOP
  ,UJ,*RELON
 :,UJ,*RELON1
  ,UJ,*RELON2
 :,UJ,*RELON3
  ,UJ,*RELON4
 :,UJ,R*VJM
  ,UJ,*ABP1A
 RELONG/:6,XTA,MESMOD
 ,U1A,*+2
 6,XTA,CON2
 6,ATX,MESMOD
 *RELON  :  6,  XTA  ,MISTABL             ПPИЗHAK ДЛ. OПEPAЦИИ
            6,  AOX  ,CN0002
            6,  ATX  ,MISTABL
             ,  UJ   ,RE8KOP
 *RELON1 :  6,  XTA  ,FLAGTABL            ПPИЗHAK ПOMEЧEHHOCTИ
            6,  AOX  ,CN0004
            6,  ATX  ,FLAGTABL
             ,  UJ   ,*RELON
 *RELON2 :  6,  XTA  ,FLAGTABL            ПPИЗH. ПEPEHOCA ПOMEЧEHHOCTИ
            6,  AOX  ,CN0004              CЛEДYЮЩEЙ ИHCTPYKЦИИ
            6,  ATX  ,FLAGTABL
             ,  UJ   ,RE8KOP
 *RELON3 :  6,  XTA  ,MISTABL
            6,  AOX  ,CN0004              BCEГДA ПOMEЧ. ИHCTPYKЦИЯ
            6,  ATX  ,MISTABL
             ,  UJ   ,RE8KOP
 R*VJM:6,XTA,FLAGTABL
 6,AOX,CN0004
 6,ATX,FLAGTABL
 ,UTC,
 *RELON4:6,XTA,MESMOD
 ,UZA,*RELON
 6,XTA,MISTABL
 6,AOX,C201S15 .LONG ADR + MOD
 6,ATX,MISTABL
 ,UJ,RE8KOP
 RELONG5:6,XTA,LONGSTR
 6,AEX,NSTR+1
 13,UZA,
 6,AEX,NSTR+1
 6,ATX,NSTR+1
 4,UTM,-30
 PL*NAM:13,UJ,
 ,UJ,PL*NAME
 R*EMC:14,Z31,R*ASC+1
 ,Z35,
 R*EPC:14,Z31,R*ASC+1
 ,Z34,
 R*ASC:14,Z31,*+1
 ,Z36,
 :6,XTA,RCON100
 6,ATX,-7
 :6,XTA,AB*PLG
 6,ATX,-8
 1,XTA,LABC-PER
 6,ATX,AB*PLG
 14,UTM,-2
 ,UJ,RE8KOP
 :6,XTA,-8
 6,ATX,AB*PLG
 6,XTA,-7
 6,ATX,MESADR3
 LABC:,UJ,READA
 ,UJ,*-2
 R*JIF:14,Z31,*+1
 1,Z10,*+6
 :6,XTA,C701S6
 6,ATX,-7
 :1,XTA,R*JIF-PER
 1,ATX,14
 :6,XTA,AB*PLG
 6,ATX,-8
 1,XTA,LABC-PER
 6,ATX,AB*PLG
 14,UTM,-2
 ,UJ,*RELON2
 TFFF:13,Z31,RELONG5
 ,UJ,FFF
 :12,VTM,2000B
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-122B
 14,VZM,JIFE+2
 14,UTM,122B-127B
 14,VZM,JIFE
 12,UTM,2000B
 14,UTM,127B-117B
 14,VZM,JIFE
 14,UTM,117B-101B
 12,UTM,2000B
 14,V1M,TO41
 JIFE:,ITA,12
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 :13,VTM,TO4
 ,UJ,*READ5
 R*STIME:14,Z31,*+3
 ,*50,70210B
 R*TIME:14,Z31,*+2
 ,*50,70077B
 R*DATE:14,Z31,*+1
 ,*50,67B
 :14,XTA,-1
 6,AAX,CONR24
 ,XTS,
 6,ATX,MESKOP
 1,XTA,TR*DATE-PER
 6,ATX,*KOPPLG
 :14,VTM,
 ,UJ,*RELON2
 TR*DATE:13,Z31,PADRESS
 ,UJ,*
 PLG73:,ATX,
 ,UTC,
 :15,XTA,
 13,VJM,COMMAND
 :6,XTA,MESMOD
 6,AOX,FLGADR
 8,VTM,1
 ,UZA,ABP1
 13,VTM,*RE8KOP8
 ,UJ,LEFCOM
 R*SIN:14,Z31,*+1
 ,*51,
 :14,XTA,-1
 6,AAX,CONR24
 6,ATX,-7
 6,XTA,CN000001
 6,ATX,MESKOP
 1,XTA,TR*SIN-PER
 6,ATX,*KOPPLG
 ,UJ,TR*DATE-1
 TR*SIN:13,Z31,PADRESS
 ,UJ,*
 :6,XTA,MESMOD
 6,AOX,FLGADR
 ,UTC,
 ,U1A,*+2
 6,XTA,-7
 ,UJ,*RE8KOP8+3
 :1,XTA,DDDD-PER
 6,ATX,*COMPLG
 DDDD:,UJ,*RE8KOP8
 ,UJ,*+1
 :6,XTA,-7
 8,VJM,*RE8KOP8+3
 R*EXP:14,Z31,R*SIN+1
 ,*56,
 R*LN:14,Z31,R*SIN+1
 ,*55,
 R*COS:14,Z31,R*SIN+1
 ,*51,1
 R*SQRT:14,Z31,R*SIN+1
 ,*50,
 R*ATAN:14,Z31,R*SIN+1
 ,*53,
 R*ASIN:14,Z31,R*SIN+1
 ,*54,
C
C**************************************
C*
C*  OБPAБOTKA ЗAГOЛOBKA ПPOГPAMMЫ.
C*
C
 R*SIBESM:6,XTA,MISTABL
 6,AAX,NCON3
 6,ATX,MISTABL
 13,VJM,IDADR
 :,XTA,
 6,XTS,MESADR1
 13,VTM,*RENS9
 ,UJ,ISKENTRY
 R*NAME:1,XTA,STEXTJMP-PER
 6,ATX,IFPLG
 ,XTA,
 6,XTS,MISTABL
 6,AAX,CON2
 15,ATX,
 6,ARX,NGZB
 15,AEX,
 6,AAX,MESMET
 13,VJM,ISKENTRY
 6,XTA,FLAGTABL
 6,AOX,RCON20
 6,ATX,FLAGTABL
 6,XTA,MESMOD
 6,AOX,GLOB*
 ,UZA,*RENS99
 6,XTA,MESMOD
 13,VTM,*RENS99
 ,UZA,GLOBERR
 1,XTA,PL*NAM-PER
 6,ATX,*KOPPLG
 ,UJ,*RE8KOM
 PL*NAME:6,XTA,GLOB*
 ,UZA,*+3
 6,AEX,MESMOD
 ,UZA,*+2
 13,VTM,GLOBERR+2
 12,V1M,GLOBERR
 :6,XTA,MESMOD
 6,ATX,GLOB*
 ,UZA,*RENA
 6,AEX,GLOBCON
 ,UTC,TABDEC
 ,ATX,1
 6,XTA,GLOB*
 6,ARX,NRC14
 6,AAX,CON1
 6,AOX,MISTABL
 6,ATX,MISTABL
 6,XTA,FLAGTABL
 6,AAX,RCON1
 6,AEX,RCON1
 6,ARX,C4
 6,ATX,C4
 *RENA:1,XTA,7
 13,VTM,*+5
 :1,ATX,13
 1,ATX,14
 1,ATX,15
 1,XTA,/6/-PER
 1,ATX,16
 ,ITA,13
 1,ATX,97
 ,UJ,ABP
 :6,XTA,INDTABL
 6,ATX,BLUTC
 13,VTM,*RENS9
 ,UJ,*WRI
 GLOBERR:6,XTA,MISTABL
 6,AAX,CON1
 13,UZA,
 13,VJM,GLOBER
 :6,XTA,GLOB*
 ,UJ,PL*NAME+4
C
C**************************************
C*
C*   OБPAБOTKA OПИCAHИЙ.
C*
C
 BL*PER:6,XTA,MESMET
 ,UJ,TOREBLK-1
 :,XTA,
 ,UJ,TOREBLK+1
 :6,XTA,CON4
 ,UJ,TOREBLK-2
 R*LC:6,XTA,CN00007
 ,UJ,RENSOP                               070S15
 R*PC:6,XTA,CN000074
 ,UJ,RENSOP                               074S15
 R*SC:6,XTA,CN00017
 ,UJ,RENSOP                               170S15
 R*LP:6,XTA,CN00006
 ,UJ,RENSOP                               060S15
 R*PP:6,XTA,CN000064
 ,UJ,RENSOP                               064S15
 R*SP:6,XTA,CN00016
 ,UJ,RENSOP                               160S15
 R*LU:6,XTA,CN00005
 ,UJ,RENSOP                               050S15
 R*PU:6,XTA,CN000054
 ,UJ,RENSOP                               054S15
 R*SU:6,XTA,CN00015
 ,UJ,RENSOP                               150S15
 R*LS:6,XTA,CN00004
 ,UJ,RENSOP                               040S15
 R*PS:6,XTA,CN000044
 ,UJ,RENSOP                               044S15
 R*SS:6,XTA,CN00014
 ,UJ,RENSOP                               140S15
 :6,XTA,MESMET
 ,UJ,BL*B
 :,UJ,R*BLOCK*+3
 R*P*P:6,XTA,CN0001
 ,UJ,REP*/P                               100S15
 R*P/P:6,XTA,CN000102
 ,UJ,REP*/P                               102S15
 R*WEQ:13,VTM,REWEQ
 ,UJ,PADRESSI                             .500S15
 R*EQU:6,XTA,FLAGTABL
 6,AAX,NCON2
 6,ATX,FLAGTABL
 6,AAX,RCON20
 12,VTM,R*EQU*
 ,UZA,RENSIB+2
 ,XTA,
 6,ATX,Q*1RENS
 6,XTA,MISTABL
 6,AOX,CN0002
 6,AAX,NCON1
 6,ATX,MISTABL
 ,UJ,RENSOP1
 R*REQU:6,XTA,MISTABL
 6,AOX,CN0002
 6,AAX,NCON1
 6,ATX,MISTABL
 6,XTA,CON002
 ,UJ,RENSOP
 R*SUBP:6,XTA,FLAGTABL
 6,AAX,NCON2
 6,ATX,FLAGTABL
 6,AAX,RCON20
 12,VTM,R*SUBP*
 ,UZA,RENSIB+2
 6,XTA,MISTABL
 6,AAX,NCON1
 6,ATX,MISTABL
 6,AAX,CON2
 ,U1A,*RENS99
 6,XTA,MESMET
 ,UZA,METERR
 6,XTA,CN00003
 6,XTS,MESMET
 13,VJM,TOTIN
 R*ENDIF:,BSS,
 *RENS99:13,VTM,*RENS9
 ,UJ,*READ3
 METERR:6,XTA,MISTABL
 6,AOX,CON2
 6,ATX,MISTABL
 ,UJ,*RENS99
 REN*GO:6,ATX,REN*FL
 6,XTA,Q*1RENS
 6,WTC,REN*FL
 ,UJ,
 RENSOP:6,ATX,Q*1RENS
 6,XTA,REN*FL
 ,U1A,REN*GO
 6,XTA,FLAGTABL
 6,AAX,NCON2
 6,ATX,FLAGTABL
 6,AAX,RCON20
 13,VTM,*RENS99
 ,UZA,NOOPRI
 6,XTA,MISTABL
 RENSOP1:6,AAX,CON2
 ,U1A,*RENS99
 6,XTA,MESMET
 6,ATX,Q*2RENS
 ,UZA,METERR
 13,VJM,PADRESSI
 *RENS6F:6,XTA,MISTABL
 6,AAX,CN0002
 ,UTC,
 ,U1A,*RENS6FL
 :6,XTA,MESADR3
 ,UZA,*RENSXA
 :6,AEX,Q*1RENS
 6,XTS,BLDEF
 :,ASN,64+7
 ,YTA,
 15,AOX,
 6,XTS,REN*FL
 ,U1A,REN*GO1
 6,XTA,Q*2RENS
 13,VTM,*RENS9
 ,UJ,TOTIN
 *RENSXA:6,XTA,XADRESS1
 ,UZA,*RENS6F+3
 ,ASN,64-17
 ,YTA,
 6,AEX,MESADR1
 ,UJ,*RENS6F+3
 *RENS6FL:6,XTA,MESADR3
 ,UZA,*+2
 :6,AEX,SYSABS
 ,UJ,*RENS6F+3
 :6,XTA,XADRESS1
 ,UZA,*-1
 6,AAX,CON1
 ,UZA,*RENSXA
 6,XTA,XADRESS1
 ,ASN,64-1
 ,YTA,
 ,ASN,64-16
 15,ATX,
 6,ARX,NGZB
 6,AAX,SYSREL
 15,AEX,
 6,AEX,CN00001
 6,AEX,MESADR1
 6,AEX,CN000004
 ,UJ,*RENS6F+3
 REN*GO1:6,XTA,Q*2RENS
 6,WTC,REN*FL
 13,VTM,
 ,UJ,TOTIN
 R*LABEL:12,VTM,R*LABEL*
 ,UJ,RENSIB
 R*ARRAY:12,VTM,R*ARRAY*
 RENSIB:6,XTA,FLAGTABL
 6,AAX,NCON2
 6,ATX,FLAGTABL
 6,AAX,RCON20
 :,U1A,*VMET6
 13,VJM,IDADR
 :6,XTA,MESADR1
 6,ATX,Q*2RENS
 6,XTA,MISTABL
 6,ATX,Q*3RENS
 6,AAX,NCON3
 6,ATX,MISTABL
 ,ITA,12
 6,ATX,Q*1RENS
 1,XTA,*RENST-PER
 6,ATX,PSKPLG
 *RENS9:,UJ,ABP1
 ,,
 :,XTA,
 6,ATX,PSKPLG
 6,XTA,MISTABL
 6,AAX,CON04
 13,VTM,POSKOP
 ,U1A,NOOPRI
 6,XTA,Q*2RENS
 ,UZA,NOENDOP
 6,WTC,Q*1RENS
 ,UJ,
 R*EQU*:6,XTA,MESKOP
 ,U1A,NOENDOP
 :6,ATX,Q*1RENS
 6,XTA,MISTABL
 6,AOX,CN0002
 6,AAX,NCON2
 6,ATX,MISTABL
 6,XTA,Q*3RENS
 6,AAX,CON02
 13,VTM,*RENS6F
 ,UZA,PADRESSI
 ,UJ,*RENS99
 R*SUBP*:6,XTA,MESKOP
 12,VTM,KOPTAB
 12,AEX,REG*T-KOPTAB
 ,UZA,*+2
 6,XTA,MESKOP
 12,AEX,LOC*T-KOPTAB
 :,U1A,NOENDOP
 6,XTA,MISTABL
 6,AAX,NCON3
 13,VTM,*RENS99
 6,ATX,MISTABL
 6,XTA,CN00003
 6,XTS,Q*2RENS
 ,UJ,TOTIN
 R*LABEL*:6,XTA,MESKOP
 12,VTM,KOPTAB
 12,AEX,D*T-KOPTAB
 ,UZA,R*EQU*+1
 6,XTA,MESKOP
 12,AEX,I*T-KOPTAB
 ,UZA,*RENS99
 ,UJ,NOENDOP
 R*ARRAY*:6,XTA,MESKOP
 6,ATX,-1
 12,VTM,-11
 ,UZA,NOENDOP
 :6,AAX,AA6SIMB
 ,U1A,*+3
 6,XTA,-1
 ,ASN,64-6
 6,ATX,-1
 ,UJ,*-2
 :6,XTA,-1
 6,UTC,RENKL+14
 12,AEX,
 ,UZA,*+2
 12,VLM,*-2
 ,UJ,NOENDOP
 :14,VTM,*+2
 ,ITA,14
 6,ATX,REN*FL
 12,UJ,R*SS
 :,XTA,
 6,ATX,REN*FL
 6,XTA,MISTABL
 6,AAX,NCON3
 6,ATX,MISTABL
 13,VTM,*RENS6F
 *RENST:,UJ,PADRESSI
 ,UJ,*RENS9+1
C
C
C         OБPAБOTKA  OCT  И  LOG
C
C
 R*LOG:1,XTA,LOGPL-PER
 1,ATX,53
 ,UTC,*RELOGC-*REOCTC
 R*OCT:1,XTA,*REOCTC-PER
 1,ATX,97
 13,VTM,ABP
 ,UJ,*MADEC1
 *REOCT1:13,VJM,*WRIN
 :6,ATX,RR1
 11,VTM,-1
 6,XTA,POSIND
 ,U1A,*REOC1
 *REOC2:6,XTS,IMIST
 ,UZA,*REOC2A
 6,XTA,MISTABL
 6,AOX,CON02
 :6,ATX,MISTABL
 ,XTA,
 *REOC2A:6,ATX,IMIST
 6,XTA,OCT*PLG
 ,U1A,*+2
 15,XTA,
 13,VTM,*TEXEND1-2
 ,UJ,*MADEC2
 :,ATI,14
 ,XTA,
 6,STX,OCT*PLG
 14,UJ,
 *CODE8:6,ATX,RR1
 6,XTA,CON1
 6,XTS,RR1
 13,VTM,CODES+2
 ,UJ,ISKKOS
 *REOC1A:6,ATX,RR1
 6,XTA,INDTABL+1
 ,UZA,*REOC1B
 6,ATX,POSIND
 *REOC1:6,ARX,NG1B
 6,AOX,POSIND
 6,AAX,MASK*20
 6,AOX,POSIND
 6,AEX,MASK*20
 6,AAX,MASK*70
 ,U1A,*REOC3
 6,XTA,POSIND
 6,APX,MASK*07
 11,VLM,*REOC1A
 ,ASN,64+24
 6,AEX,RR1
 6,ATX,RR1
 ,UJ,*REOC2
 *REOC1B:6,XTA,RR1
 ,UJ,*REOC2
 *REOC3:6,XTA,RR1
 6,XTS,MISTABL
 6,AOX,CON02
 ,UJ,*REOC2+2
 *REOCT2:6,XTA,INDTABL+1
 ,U1A,*REOC4
 6,XTA,INDTABL
 ,UZA,*REOCTSH-1
 13,VJM,PER6T8
 6,XTS,RMIST
 6,AOX,IMIST
 6,ATX,IMIST
 13,VJM,*WRI
 6,STX,RMIST
 ,UJ,*REOC2S
 *REOCTMI:,XTA,
 6,ATX,SIGNE
 *REOCTPL:1,XTA,7
 1,ATX,13
 1,ATX,15
 6,XTA,INDTABL
 6,AOX,IMIST
 6,ATX,IMIST
 ,UJ,ABP
 SHITS1:15,XTA,
 6,STX,OCTAL
 6,ATX,DECIMAL
 6,XTA,OCTAL
 6,XTS,DECIMAL
 15,ATX,
 SHF1:6,XTA,INDTABL
 13,VTM,*+2
 ,U1A,*WRIN
 ,UJ,SHF2
 6,XTA,POSIND
 13,VJM,PERI10
 6,XTS,RMIST
 6,AOX,IMIST
 6,ATX,IMIST
 ,XTA,
 6,STX,RMIST
 ,UZA,SHF2
 6,ARX,SIGNE
 6,AEX,SIGNE
 ,STI,14
 14,ASN,64
 15,ATX,
 ,YTA,
 15,AEX,
 ,UJ,*REOC2
 SHF2:,STX,
 ,UJ,*REOC2
 *REOC4:6,XTA,INDTABL
 6,ATX,RR1
 5,VZM,*REOC5
 12,VTM,64-6
 :12,UTM,6
 5,VLM,*
 15,ATX,
 12,ASN,
 6,ATX,RR1
 ,YTA,
 6,AEX,INDTABL+1
 6,STX,INDTABL+1
 *REOC5:6,AEX,MASK*20
 6,AAX,MASK*70
 2,VTM,STROKA+INDTABL
 ,XTS,
 6,ATX,INDTABL
 6,XTA,INDTABL+1
 6,AEX,MASK*20
 6,AAX,MASK*70
 5,VTM,-8
 15,AOX,
 6,AOX,IMIST
 6,ATX,IMIST
 6,XTA,RR1
 6,APX,MASK*07
 6,XTS,INDTABL+1
 6,APX,MASK*07
 ,ASN,64+24
 15,AEX,
 LOGPER:,EQU,LOGPL-PER
 *REOC2S:1,XTS,53
 1,AEX,LOGPER+3
 ,UZA,ABP
 1,XTA,LOGPER+3
 1,STX,53
 ,UJ,*REOC2
 :1,XTS,LOGPER+3
 ,UJ,*-1
 *REOCTSH:1,XTA,LOGPER+3
 1,ATX,53
 1,XTA,LOGPER+1
 1,ATX,15
 1,XTA,LOGPER+2
 1,ATX,13
 1,XTA,LOGPER+4
 1,ATX,97
 6,XTA,CONR15
 6,ATX,SIGNE
 ,UJ,*REOCT2
C
C**************************************
C*
C*  OБPAБOTKA 6TEXT И 8TEXT (SIBESM-6).
C*
C
 R*6TEXT:,UTC,OUT6-OUT8
 R*8TEXT:7,VTM,
 13,VJM,*MADEC1
 :12,VTM,-1
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-47B
 14,V1M,*TEXEND1
 12,VLM,READ
 12,VTM,-48
 13,VJM,READ
 :14,UTM,-177B
 14,VZM,*TEXEND
 14,UTM,177B-47B
 14,VZM,*TEXEND2
 14,UTM,47B
 7,UJ,*+1
 OUT8:,ITA,14
 6,XTS,RR1
 ,ASN,64-8
 12,UTM,8
 15,AEX,
 6,ATX,RR1
 12,V1M,READ
 13,VJM,READ
 :14,UTM,-177B
 14,VZM,*TEXEND
 14,UTM,177B-47B
 14,V1M,READ
 *TEXEND2:13,VJM,READ
 14,UTM,-177B
 14,VZM,*TEXEND
 14,UTM,177B-40B
 14,VZM,READ
 14,UTM,40B-47B
 13,VTM,*TEXEND
 14,VZM,*READ3
 ,UJ,EDITTT
 *TEXEND:6,XTA,RR1
 12,ASN,64
 :13,VJM,*MADEC2
 :6,XTA,LEFINST
 13,VTM,ABP1
 TRISWRTL:,UJ,COMCNPR
 ,UJ,REISWRTL
 *TEXEND1:6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 13,VJM,*READ4
 ,XTA,
 ,UJ,*TEXEND+1
 OUT6:14,UTC,ABP+2
 ,XTA,
 ,ASN,64+24
 6,AAX,AA6SIMBR
 6,XTS,RR1
 ,ASN,64-6
 12,UTM,6
 ,UJ,OUT8+2
C
C**************************************
C*
C*  BCПOMOГATEЛЬHЫE OПEPAЦИИ.
C*
C
 *VMET1:6,XTA,MISTABL
 6,AAX,NCON3
            6,  ATX  ,MISTABL
           13,  UJ   ,
 *VMET6  :  6,  XTA  ,MISTABL
 6,AAX,NCON76
 6,ATX,MISTABL
 ,UJ,*ABP1A
C
C**************************************
C*
C*  CTAHДAPTHAЯ ПPOЦEДYPA ЗAПИCИ
C*  6-БИTOBЫX CИMBOЛOB (KOД TEXT).
C*
C
 WRITE:,UTC,ABP+2
      14,XTA,
             ,  ASN  ,64+24
 6,AAX,AA6SIMBR
 :5,VZM,*WRITE1
 2,XTS,
             ,  ASN  ,64-6
           15,  AEX  ,
            2,  ATX  ,
            5,  VLM  ,ABP
 *WRITE1:5,UTM,-7
 2,ATX,1
 2,VLM,ABP
 *WRIN:2,XTA,
 5,VZM,*WRI
 ,UZA,*WRI
 5,J+M,5
 5,ASN,64
 5,J+M,5
 5,ASN,64
 2,ATX,
 *WRI:6,XTA,INDTABL
 6,ATX,POSIND
 ,XTA,
 6,ATX,INDTABL
 2,VTM,STROKA+INDTABL
 5,VTM,-8
           13,  UJ   ,
C
C**************************************
C*
C*  OБPAБOTKA KOHCTAHT
C*  ISO,GOST,TEXT,TEL.
C*
C
 R*TEXT:1,XTA,TISOTEXT-PER
 1,XTS,TREISWR4-PER
 6,XTS,RCON10
 6,XTS,CON43
 TISOGOST:,UJ,R*ISO+2
 ,UJ,ISOGOST
 R*TEL:1,XTA,TISOTLG-PER
 ,XTS,
 6,ATX,RE*RREG
 6,ATX,RE*RTLG
 1,XTA,TRISWRTL-PER
 ,UJ,R*ISO+1
  *REISA:1,XTA,*REIHHH-PER
 1,ATX,42
 6,XTA,INDTABL
 6,ATX,POSIND
 ,UZA,*DELIM
 13,VJM,*WRIN
            6,  XTA  ,POSIND
           13,  VJM  ,PERI10              ПEPEBOД N
            6,  ATX  ,POSIND
            6,  XTA  ,RMIST
 T*REISB:6,AOX,IMIST
             ,  U1A  ,*REISB              OШИБKA
            6,  XTA  ,POSIND
             ,  ASN  ,64+7
             ,  U1A  ,*REISB              N>128
 6,ATX,RE*RAB
 6,ATX,RE*R
            6,  XTA  ,POSIND
 :,U1A,*REISA1
           12,  VTM  ,*PRREAD
             ,  ITA  ,12
 6,ATX,REISRR1
 6,XTA,RCON6
            6,  ATX  ,POSIND
 *REISA1:6,WTC,REISRR1
           13,  VJM  ,
 6,XTA,POSIND
           14,  MTJ  ,13
           13,  UTM  ,-177B
 ,UZA,*DELIM1
 :13,VZM,*REISK
 13,UTM,177B-47B
 13,VZM,*REISA2
 ,ITA,14
 6,WTC,REISRR4
           13,  VJM  ,                    ПEPEBOД CИMBOЛA
 6,XTS,NG1B
 6,STX,TELLI
 *REISA21:6,WTC,REISRR5
           13,  VJM  ,
             ,  U1A  ,*REISA2A            CЛOBO HE ЗAПOЛHEHO
           13,  VJM  ,REISPRI             OБP. CЛOBA (ЗAПИCЬ И ПEЧATЬ)
 *REISA2A:6,XTA,RE*R
 6,ARX,RCON1
 6,ATX,RE*R
 6,XTA,COMMA
 ,UZA,*+3
 6,XTA,RE*R
 6,AEX,POSIND
 ,U1A,*REISA2
 6,ATX,COMMA
 ,UJ,*REISB
 :6,XTA,RE*R
            6,  AEX  ,POSIND
             ,  U1A  ,*REISA1             HE N
 13,VJM,*READ3
 *REISA3:6,XTA,RE*RAB
 13,VTM,*ABP1
 ,UZA,BSSPRINT
 ,XTA,
 T*REISC:,UJ,*REISA*3+2
 ,UJ,*REISC
 *REISA2:6,XTA,REISRR2
 1,XTS,T*REISB-PER
 TISOTEXT:13,Z31,APOSTLIT
 ,UJ,ISOTEXT
 :,XTS,
 6,STX,TELLI
 TISOISO:,UJ,*REISA21
 ,UJ,UJ13
 *DELIM:6,WTC,REISRR1
 13,VJM,
 ,XTA,
 6,ATX,RE*RAB
 14,UTM,-177B
 6,ATX,RE*R
 14,VZM,*REISA1-3
 ,ITA,14
 6,ATX,DECIMAL
  ,UJ,*REISA1
 *DELIM1:,ITA,13
 6,AOX,RE*R
 ,UZA,*REISA1-3
 13,VZM,*REISA3
 ,ITA,13
 6,AEX,DECIMAL
 ,U1A,*REISA1+3
 ,UJ,*REISK-1
 R*GOST:,UTC,TISOGOST-TISOISO
 R*ISO:1,XTA,TISOISO-PER
 1,XTS,TREISWR1-PER
 :6,XTS,RCON6
 6,XTS,CON44
 :6,STX,REISRR2
 6,STX,Q*1RENS
 6,STX,REISRR5
 6,ATX,REISRR4
 6,XTA,FLAGTABL
 6,AAX,CON1
 ,U1A,*MACAB
 6,ATX,REISSYM
 6,ATX,REISCS
 13,VJM,LEFCOM-1
 :1,XTA,T*REISC-PER
 6,ATX,PSKPLG
 *REISO*:12,VTM,*REISA
 ,ITA,12
 1,ATX,42
 13,VJM,RELONG5
 :1,XTA,READRET-PER
 6,ATX,REISRR1
 1,XTA,7
 13,VJM,*RENA+1
 ,XTA,
 6,ATX,RE*RAB
 6,ATX,RE*R
 13,VJM,*WRI
 *REISB:1,XTA,*REIHHH-PER
            1,  ATX  ,42
             ,  XTA  ,
            6,  ATX  ,RMIST
            6,  ATX  ,IMIST
            6,  XTA  ,MISTABL
            6,  AOX  ,CON02
            6,  ATX  ,MISTABL
 :13,VTM,*REISA3
 ,UJ,*READ3
 *REISK  : 12,  VTM  ,*PRREAD             CИMBOЛ-ПPOБEЛ
             ,  ITA  ,12
 6,ATX,REISRR1
 ,UJ,*REISA1
 *REISC:6,XTA,MESKOP
 13,VTM,*REISO*
 13,AEX,CONT*T-*REISO*
 ,UZA,MET
 ,XTA,
 6,ATX,PSKPLG
 6,XTA,REISCS
 ,U1A,REISPR
 6,XTA,RE*RAB
 ,U1A,*REISA3*+1
 REISPR:6,XTA,RCON40
 6,WTC,REISRR4
 TREISWR4:13,Z31,
 ,UJ,REISWR1-1
 :6,XTS,NG1B
 6,STX,TELLI
 6,WTC,REISRR5
 13,VJM,
 ,U1A,REISPR
 6,ATX,RE*RAB
 6,ATX,RE*RAB1
 6,XTA,REISSYM
 13,VTM,PLG20
 ,UJ,*MADEC2
 *REISA*3:6,XTA,RE*RAB1
 6,AEX,RE*RAB
 ,UZA,*REISA3*
 6,XTA,RE*RAB1
 :6,ARX,RCON1
 6,ATX,RE*RAB1
 ,ATI,13
 13,UTC,SPASAN*
 ,XTA,-1
 13,VJM,*MADEC2
 PLG20:13,VTM,*REISA*3
 ,UJ,*MACAF
 6,XTA,RE*RAB1
 6,AEX,RCON1
 13,VTM,*REISA*3
 ,UZA,*REISA3*-1
            6,  XTA  ,NK
            6,  ARX  ,CONR15
            6,  AAX  ,CONR15
            6,  ATX  ,NK
 6,XTA,LEFINST
 13,VJM,CONSTPRI
 :6,XTA,NK
            6,  ARX  ,RCON1
            6,  ATX  ,NK
             ,  UJ   ,*REISA*3
 :6,XTA,LEFINST
 ,UJ,COMCNPR
 *REISA3*:6,XTA,RE*RAB
 ,U1A,*ABP1
 :6,XTA,NK
 6,AEX,NKKOS
 13,VTM,POSKOP
 13,U1A,
 TISOTLG:,UJ,LITWRITE
 ,UJ,ISOTLG
 REISPRI:6,XTA,RE*RAB
 6,ARX,RCON1
 6,ATX,RE*RAB
 ,ATI,14
 6,XTA,REISSYM
           14,  UTC  ,SPASAN*-1
             ,  ATX  ,
           13,  UJ   ,
 *PRREAD : 14,  VTM  ,32
 13,UJ,
 :6,XTS,REISSYM
 ,UJ,*+2
 REISWR1:6,XTS,REISSYM
 ,ASN,64-2
 :,ASN,64-6
 15,AEX,
 6,ATX,REISSYM
 6,XTA,REISCS
 6,ARX,RCON1
 6,ATX,REISCS
 6,AEX,Q*1RENS
 13,U1A,
 6,ATX,REISCS
 13,UJ,
 REISWRTL:6,WTC,TELLI
 14,VTM,
 6,ATX,RR3
 14,VZM,REISWR1
 ,ASN,64+5
 ,U1A,WRTL
 :14,UTC,2
 6,XTA,TLREGC
 6,AEX,RR3
 ,UZA,WRTL00
 14,VLM,*-2
 ,UJ,WRTLA
 WRTL00:6,XTA,RCON200
 14,ASN,64+2
 6,ATX,RE*RREG
 ,UJ,WRTLA
 WRTL:6,XTA,RE*RREG
 6,AAX,RR3
 ,U1A,WRTL1
 6,XTA,RE*RTLG
 ,U1A,TLERR
 6,XTA,RR3
 6,AAX,RCON40
 ,ASN,64-1
 6,AAX,RR3
 6,AEX,RR3
 6,AAX,RCON340
 6,ATX,RE*RREG
 6,AEX,RCON340
 ,ASN,64+6
 6,XTS,RCON40
 15,WTC,
 6,AEX,TLREGC-1
 ,ASN,64-2
 13,MTJ,7
 13,VJM,REISWR1
 :,U1A,*+1
 13,VJM,REISPRI
 :7,MTJ,13
 ,UTC,
 WRTL1:6,XTA,RE*RREG
 6,AEX,RCON100
 ,U1A,WRTL2
 6,XTA,RR3
 6,AAX,TL0
 6,AEX,TL0
 ,U1A,WRTL2
 6,XTA,RR3
 14,VTM,-11
 6,AEX,TLGRYS
 :6,AAX,TLREGR
 ,UZA,*+3
 6,XTA,RR3
 14,UTC,TLGRYS+11
 6,AEX,1
 14,VLM,*-2
 :14,UTC,TLGRYS+11
 6,XTA,
 ,ASN,64+6
 6,ATX,RR3
 WRTLA:6,XTA,RR3
 6,AEX,RCON40
 TREISWR1:,ASN,64-2
 ,UJ,REISWR1
 WRTL2:6,XTA,RR3
 6,AAX,TLREGR
 6,ATX,RR3
 ,UJ,WRTLA
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ BLOCK.
C*
C
 R*BLOCK:,XTA,
 6,ATX,IRE
 6,ATX,BLDEFB
 6,ATX,RE*RREG
 6,ATX,TELLI
 6,ATX,REISCS
 6,ATX,REISSYM
 6,ATX,RE*RAB
 6,ATX,RE*RAB1
 13,VJM,RELONG5
 :1,XTA,REBLG-PER
 6,ATX,PSKPLG
 6,XTA,MISTABL
 6,AAX,CON3
 ,U1A,BL**EQ
 6,XTA,MESMOD
 ,UZA,BL*
 14,VTM,-16
 :6,UTC,RENKL+16
 14,AEX,
 ,UZA,R*BLOCK*-1
 6,XTA,MESMOD
 14,VLM,*-2
 6,XTA,CON1
 *REBL1:6,AOX,MISTABL
 7,VTM,*REBLB
 TOREBLJ2:,UJ,REMIS+1
 ,UJ,REBLJ2
 :1,XTA,R*BLOCK*-PER
 6,ATX,REN*FL
 R*BLOCK*:14,UJ,BL*PER+16
 ,UJ,*+1
 :6,XTS,MESMET
 13,VJM,TOTIN
 :6,XTA,CON2
 6,ATX,TELLI
 :6,XTA,MESMET
 13,VJM,ISKADR
 :6,STX,RE*RAB1
 ,UZA,*+8
 6,AAX,CONR15
 15,UTM,1
 6,ATX,RE*RAB
 ,ASN,64-36
 6,STX,VERET
 6,AEX,RE*RAB
 6,ATX,RE*RREG
 ,NTR,13B
 ,U1A,*+3
 6,XTA,RE*RAB1
 6,ARX,CONR15
 6,AEX,CONR15
 6,AAX,CONR15
 6,ATX,RE*RAB1
 :6,XTA,BLDEF
 6,ATX,BLDEFB
 6,XTA,IRE
 6,AAX,CONR15
 6,ATX,IRE
 12,VZM,*REBLB-3
 :6,XTA,REISCS
 ,ASN,64+2
 6,AAX,RE*RREG
 ,ASN,64-1
 6,AOX,MISTABL
 6,ATX,MISTABL
 :6,XTA,TELLI
 ,U1A,*REBLB
 :6,XTA,RE*RAB1
 ,UTC,
 :6,AEX,RE*RREG
 6,ATX,IRE
 *REBLB:1,XTA,7
 1,ATX,13
 1,ATX,15
 1,XTA,/6/-PER
 TOREBLJ:1,ATX,16
 7,VTM,REBLJ
 *REBLB*:6,XTA,RCON1
 6,ATX,RE*RTLG
 1,XTA,REBLI-PER
 1,ATX,14
 1,XTA,TOREBLJ-PER
 1,ATX,97
 1,XTA,TOREBLK-PER
 1,ATX,10
 REBLI:,UJ,ABP
 ,UJ,*+1
 :6,XTA,INDTABL
 7,VTM,REBLI*
 REBKKII:,UZA,*REBIIZ
 13,VJM,*WRIN
 :6,XTA,REISSYM
 12,VTM,*REBII
 ,UZA,LOI
 6,XTA,POSIND
 12,UJ,
 REBLI*:13,VTM,*REBLB*
 ,UJ,REBFRAG
 BL*:6,XTA,MESMET
 ,UZA,*REBLB
 ,ASN,64-6
 13,VJM,PERI10
 *REB2A:6,ATX,RE*RAB1
 6,XTA,RMIST
 ,UZA,*REBLB-2
 6,XTA,CON2
 REBLI1:,UJ,*REBL1
 ,UJ,*+1
 :7,VTM,REBFRAG
 13,VTM,*REBLB*
 REBLJ1:,UJ,REMIS
 ,UJ,*+1
 :13,VTM,REBLJ+1
 ,UJ,REMIS
 BL**EQ:6,AAX,CON1
 ,U1A,BL**ER
 6,XTA,MESMOD
 6,AEX,RENKL
 :,U1A,*REBL*W
 6,XTA,NUMMET
 ,UZA,*REBL*Z
 6,XTA,MISTABL
 6,AAX,NCON6
 6,ATX,MISTABL
 6,XTA,ASTNUMET
 6,AAX,AA6SIMBR
 13,VTM,*REB2A
 6,AEX,BSIMB
 ,U1A,*+2
 6,XTA,ASTNUMET
 ,ASN,64+6
 ,UJ,PER6T8
 :6,XTA,NUMMET
 ,UJ,PERI10
 *REBL*Z:6,XTA,ASTMET
 ,U1A,*REBLB
 6,XTA,CON1
 6,ATX,RE*RREG
 6,XTA,NK
 6,ATX,RE*RAB
 :6,XTA,MISTABL
 6,AAX,NCON6
 6,ATX,MISTABL
 ,UJ,*REBLB-6
 *REBL*W:6,XTA,MESMOD
 6,AEX,RENKL+2
 ,U1A,*REBL*SB
 6,XTS,CON4
 6,STX,REISCS
 ,UJ,BL**EQ+2
 *REBL*SB:6,XTA,MESMOD
 6,AEX,RENKL+1
 6,AOX,ASTMET
 ,U1A,*REBLB
 6,ATX,REISRR4
 6,XTA,FLAG3B
 6,ATX,REISRR1
 6,XTA,CON43
 6,ATX,REISRR2
 6,XTA,CN00003
 6,ATX,REISSYM
 6,XTA,CN00007
 6,ATX,REISRR5
 ,UJ,*REBL*Z+3
 BL**ER:6,XTA,MISTABL
 6,AAX,CON2
 ,UZA,*REBLB
 6,XTA,NUMMET
 TREBISB1:,U1A,*+2
 12,VTM,REBLK1A-1
 6,XTA,ASTMET
 ,U1A,*REBLB
 :6,XTA,MISTABL
 6,AAX,NCON2
 6,ATX,MISTABL
 ,UJ,*REBLB
 :6,ATX,REISCS
 6,XTA,MESMET
 :,UZA,*REBLB
 13,VTM,R*BLOCK*+4
 TOREBLK:,UJ,ISKADR
 ,UJ,REBLK
 :6,ATX,REISRR4
 6,ATX,REISRR2
 6,XTA,CN00003
 6,ATX,REISSYM
 6,XTA,CN00007
 6,ATX,REISRR5
 6,XTA,MESMET
 6,ATX,REISRR1
 14,VTM,100B
 ,UJ,*+2
 :,ASN,64-6
 14,UTM,6
 :,U1A,*-1
 ,ITA,14
 ,ASN,64-41
 6,ATX,REISRR2
 6,XTA,MISTABL
 6,AAX,CON6
 ,UZA,*REBLB
 6,XTS,ASTMET
 ,U1A,*+3
 6,XTA,FLAG3B
 6,STX,REISRR1
 6,AEX,MISTABL
 6,ATX,MISTABL
 ,UJ,*REBLB
 :6,AAX,NCON77
 6,AEX,CON01
 6,STX,REISRR1
 ,UJ,*REBLB
 BL*B:,UZA,*REBLB
 6,XTA,NGZB
 6,ATX,RR1
 13,VTM,*REB2A
 :6,XTA,RR1
 ,ASN,64+6
 6,ATX,RR1
 6,AAX,ASTNUMET
 6,AEX,ASTNUMET
 ,UZA,*-2
 6,AEX,ASTNUMET
 ,UJ,PER6T8
 :6,XTA,MESKOP
 ,UTC,CONT*T
 ,AEX,
 13,VTM,*REBLB
 ,UZA,RELONG5
 6,XTA,TELLI
 6,AAX,RE*RREG
 ,UTC,
 ,UZA,*REBLG
 6,WTC,RE*RAB
 14,VTM,TABDEC-4000B
 14,XTA,
 6,AEX,RE*RAB1
 14,ATX,
 *REBLG:,XTA,
 6,ATX,PSKPLG
 6,ATX,REN*FL
 ,UJ,POSKOP+1
 REBFRSB:6,XTS,REISRR4
 ,U1A,*+3
 :6,XTA,Q*2RENS
 6,ASX,REISRR2
 6,AEX,REISRR1
 ,UJ,TOTIN
 :6,AEX,RE*RTLG
 15,UTM,-1
 ,XTS,
 6,ATX,REISRR4
 REBLG:,UJ,*-4
 ,UJ,BL*B+6
 REBLJ:6,XTA,INDTABL
 7,VJM,REBKKII
 :1,XTA,7
 1,ATX,10
 PLG21:1,ATX,11
 13,VTM,ABP1
 REBFRAG:6,XTA,RE*RAB1
 6,ATX,MESADR3
 6,ARX,RE*RTLG
 6,ATX,RE*RAB1
 6,XTA,RE*R
 ,U1A,*+2
 :6,ATX,REISRR4
 13,UJ,
 :6,ATX,Q*2RENS
 6,XTA,REISSYM
 ,U1A,REBFRSB
 ,ITA,13
 6,ATX,REN*FL
 6,XTA,BLDEFB
 6,ATX,BLDEF
 6,XTA,RE*RAB
 6,ATX,MESADR1
 6,XTA,RE*RREG
 6,ATX,XADRESS1
 6,XTA,REISCS
 7,VTM,REWEQQ
 ,U1A,ENDADR5
 6,ATX,Q*1RENS
 6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 7,VTM,*RENS6FL
 ,UJ,ENDADR5
 ENDREB:6,XTA,IRE
 6,AAX,CONR15
 6,ATX,MESADR5
 6,AEX,IRE
 6,ATX,XADRESS5
 6,XTA,RE*RAB1
 6,AEX,RE*RREG
 6,ATX,IRE
 13,VTM,ABP1
 ,UJ,DECPRINT
 *REBIIZ:6,ATX,RE*RTLG
 6,ATX,RE*R
 :6,XTA,IMIST
 7,UZA,
 REMIS:6,XTA,MISTABL
 6,AOX,CON02
 :6,ATX,MISTABL
 ,XTA,
 6,ATX,IMIST
 6,ATX,RMIST
 6,ATX,RE*RTLG
 7,UJ,
 REBLK:6,XTA,REISSYM
 7,VTM,REBNOZ
 13,VTM,*REBIISB
 ,U1A,*WRIN
 6,XTA,INDTABL
 13,VTM,LOI
 ,UZA,*REBIIZ
 12,VJM,*WRIN
 *REBII:6,ATX,RE*R
 6,XTA,IMIST
 :7,UZA,
 ,XTA,
 6,ATX,RE*R
 ,UJ,REMIS
 *REBIISB:6,XTA,IMIST
 ,U1A,*-2
 6,XTA,POSIND
 6,ATX,RE*R
 ,UZA,*+2
 6,XTA,REISRR5
 6,ATX,REISRR4
 7,UJ,
 :1,XTA,REBLK0-PER
 1,ATX,11
 REBLK0:,UJ,REBNOZ+1
 ,UJ,*REBISB
 REBNOZ:1,XTA,REBLK1-PER
 1,ATX,11
 1,XTA,REBLI1-PER
 1,ATX,14
 1,XTA,REBLJ1-PER
 1,ATX,97
 1,XTA,7
 1,ATX,10
 REBLK1:,UJ,ABP
 ,UJ,*+1
 :6,XTA,INDTABL
 7,VTM,REBLK1A
 6,ATX,RR1
 13,VJM,*WRIN
 6,XTA,IMIST
 ,U1A,REMIS
 6,XTA,POSIND
 ,UZA,REBLK1B
 6,AEX,CON2
 ,ASN,64+1
 6,ARX,CON33
 ,UZA,REBIND
 6,XTA,RR1
 6,AAX,AA6SIMBR
 6,AEX,BSIMB
 ,UZA,REBIB
 6,XTA,POSIND
 13,VJM,PERI10
 REBLK1B:6,ATX,RE*RTLG
 6,XTA,RMIST
 7,UZA,
 ,UJ,REMIS
 REBIB:6,XTA,RR1
 13,VTM,REBLK1B
 ,ASN,64+6
 ,UJ,PER6T8
 REBIND:6,XTA,POSIND
 6,AEX,FLAG3B
 ,UZA,REMIS
 6,XTA,POSIND
 15,ATX,
 13,VJM,ISKADR
 :6,STX,RE*RTLG
 15,UTM,-1
 7,UZA,
 ,ITA,
 15,UTM,1
 6,STX,RE*RTLG
 7,UTC,
 12,VZM,
 7,MTJ,13
 ,UJ,NOABSI
 *REBISB:6,XTA,IMIST
 13,VTM,*+2
 ,UZA,*WRIN
 ,UJ,REMIS
 :6,XTA,POSIND
 14,VTM,-11
 6,UTC,RENKL+14
 14,AEX,
 ,UZA,*+2
 6,XTA,POSIND
 14,VLM,*-2
 ,UJ,REMIS
 :1,XTA,TREBISB1-PER
 6,ATX,REN*FL
 REBLI2:14,UJ,R*LC+11
 ,UJ,*+1
 :13,VTM,*REBLB*
 7,VTM,REBFRAG
 REBLJ2:6,XTA,INDTABL
 6,AOX,IMIST
 7,UZA,
 ,UJ,REMIS
 :6,ATX,REISRR5
 REBLK1A:1,XTA,7
 1,ATX,11
 1,XTA,REBLI2-PER
 1,ATX,14
 1,XTA,TOREBLJ2-PER
 7,VTM,REBLJ+1
 1,ATX,97
 ,UJ,ABP
C
C**************************************
C*
C*   ПAPAMETPИЧECKИE ЭKBИBAЛEHTHOCTИ
C*                 P*P  И  P/P
C*
C
 REP*/P:6,ATX,Q*1RENS
 6,ATX,*EQPLG
 1,XTA,TREP*C1-PER
 1,ATX,14
 13,VJM,PADRESSI
 :6,ATX,*EQPLG
 6,ATX,MESADR5
 6,ATX,XADRESS5
 6,XTA,TSCF
 6,AEX,CON4
 13,VTM,REP*ABS-1
 ,U1A,*READ3
 6,XTA,MESADR3
 6,ATX,Q*2RENS
 ,U1A,*+3
 6,XTA,MESADR1
 6,AAX,RMASK11
 ,UZA,*+2
 6,XTA,MESADR1
 :,ASN,64-24
 6,AEX,Q*1RENS
 :6,ATX,Q*1RENS
 1,XTA,TSC0-PER
 1,ATX,10
 13,VJM,PADRESSI
 :6,ATX,MESADR5
 6,ATX,XADRESS5
 6,XTA,Q*1RENS
 ,UZA,REP*OUT
 6,XTA,MESADR1
 6,AAX,RMASK11
 6,AOX,MESADR3
 ,UZA,REP*OUT
 6,XTA,MESADR3
 ,UZA,*+2
 6,XTA,Q*2RENS
 ,U1A,REP*ABS
 :6,XTA,MESADR3
 ,U1A,*+2
 6,XTA,MESADR1
 6,AEX,CNR104S9
 :6,AEX,Q*1RENS
 6,ATX,Q*1RENS
 6,XTA,Q*2RENS
 6,AAX,MASK37
 ,UZA,*+4
 6,XTA,Q*1RENS
 6,AAX,CONR24
 6,ATX,Q*1RENS
 6,XTA,Q*2RENS
 13,VJM,SSILKC
 :,ASN,64-24
 ,UTC,
 :6,AEX,Q*1RENS
 13,VJM,SSILKD
 :6,XTS,CON2
 6,STX,XADRESS5
 :,ASN,64-24
 ,UTC,
 :6,AEX,CN000001
 6,ATX,Q*1RENS
 REP*OUT:6,XTS,MESMET
 6,ATX,Q*2RENS
 ,UZA,*+2
  6,XTA,BLDEF
 ,UJ,*RENS6F+4
 :6,XTA,MISTABL
 6,AOX,CON2
 6,STX,MISTABL
 6,XTA,REN*FL
 ,UZA,*RENS9
 ,UJ,REN*GO
 :,XTA,
 ,UJ,REP*OUT
 REP*ABS:6,AEX,CINT0
 ,NTR,6
 ,E+N,100B
 6,XTS,MESADR3
 6,AEX,CINT0
 ,E+N,100B
 6,ATX,MESADR3
 6,XTA,Q*1RENS
 6,AAX,CN000002
 ,UTC,
 ,U1A,*+2
 15,XTA,
 6,A*X,MESADR3
 ,UJ,*+2
 :15,XTA,
 6,A/X,MESADR3
 :,NTR,3
 6,A+X,CINT0
 6,AAX,CONR15
 6,ATX,MESADR5
 6,ATX,Q*1RENS
 ,UJ,REP*OUT
C
C**************************************
C*
C*   ПAPAMETPИЧECKAЯ ЭKBИBAЛEHTHOCTЬ WEQ
C*
C
 REWEQ:6,XTA,MESADR3
 13,VTM,REP*OUT-2
 ,U1A,*+4
 6,XTA,MESADR4
 7,VTM,*+1
 ,U1A,REMIS
 :6,XTA,MESADR1
 6,AAX,RMASK11
 13,UZA,2
 6,AEX,CN000001
 :6,AEX,CN0005
 ,UJ,SSILKD
 REWEQQ:6,XTA,MESADR3
 13,VTM,*+2
 ,UZA,REWEQ+3
 ,UJ,REWEQ+5
 :,ASN,64-24
 ,UTC,
 :6,AEX,CN000001
 6,ATX,Q*1RENS
 :6,XTS,BLDEF
 ,UJ,*RENS6F+4
C
C**************************************
C*
C*   YCЛOBHAЯ TPAHCЛЯЦИЯ.
C*
C
 IF*TSC:14,VTM,KOPTAB
 14,XTA,ELSE*T-KOPTAB
 6,AEX,MESKOP
 12,VTM,IF*COUNT
 13,VTM,ERRIF
 ,UZA,*KOPON
 14,XTA,END*T-KOPTAB
 6,AEX,MESKOP
 ,UZA,TOEND
 14,XTA,NAME*T-KOPTAB
 6,AEX,MESKOP
 ,UZA,TONAME
 IF*COUNT:6,XTA,RCON1
 6,ARX,IFCNT
 6,ATX,IFCNT
 6,AEX,IFCNTM
 ,U1A,SKIP*P
 13,VTM,SKIP*P
 OUTSKIP:,XTA,
 6,ATX,IFPLG
 6,ATX,SKIPFLAG
 13,UJ,
 TOEND:13,VTM,R*END
 ,UJ,OUTSKIP
 IF*TS:14,VTM,KOPTAB
 14,XTA,ELSE*T-KOPTAB
 6,AEX,MESKOP
 13,VTM,ERRIF
 ,UZA,*KOPON-1
 14,XTA,ENDIF*T-KOPTAB
 6,AEX,MESKOP
 ,UZA,*KOPON-2
 14,XTA,END*T-KOPTAB
 6,AEX,MESKOP
 ,UZA,TOEND
 14,XTA,NAME*T-KOPTAB
 6,AEX,MESKOP
 ,U1A,SKIP*P
 TONAME:13,VTM,*VMET6
 ,UJ,OUTSKIP
 :12,VTM,SKIP*P
 ,UJ,*+2
 :14,VTM,
 12,VTM,SKIP*P
 *KOPON:6,XTA,MISTABL
 6,AAX,CON3
 ,U1A,OUTSKIP
 6,XTA,MESMET
 6,AEX,IFNAM
 ,UZA,*+3
 6,XTA,MESMET
 ,UZA,*+2
 6,XTA,IFNAM
 12,U1A,
 :14,VZM,TERMSKP
 13,VJM,*READ3
 :13,VTM,IF*P
 ,UJ,OUTSKIP
 TERMSKP:13,VJM,*READ3
 :13,VTM,TERMSKP-1
 ,UJ,COM*M
 SKIP*P:6,XTA,LONGSTR
 6,ATX,NN
 6,XTA,MISTABL
 6,AAX,NCON76
 6,ATX,MISTABL
 13,VJM,*READ4
 :13,VTM,ABP1
 ,UJ,SKIPPR
 R*ELSE:13,VJM,*READ3
 :13,VTM,REELSE1
 ,UTC,
 COM*M:,ITA,13
 1,XTS,PL*IFM-PER
 6,ATX,*KOPPLG
 6,XTA,MASK33
 6,ATX,MODMASK
 ,UJ,*RE8KOM
 REELSE*:6,XTA,RR2
 ,UZA,*+2
 6,XTA,IRB
 13,VJM,NODEFI
 15,WTC,
 ,UJ,
 REELSE1:6,XTA,MISTABL
 6,AAX,CON72
 ,U1A,ERRIF
 6,ATX,IFCNT
 6,ATX,IFCNTM
 6,XTA,MESMOD
 6,ATX,MESADR5
 ,UZA,NAMEIF
 6,ATX,IFCNTM
 1,XTA,PLUGTSC-PER
 :6,ATX,SKIPFLAG
 6,ATX,IFPLG
 IF*P:6,XTA,MESMET
 6,ATX,IFNAM
 13,VTM,ABP1
 ,UJ,IFPR
 NAMEIF:1,XTA,PLUGTS-PER
 ,UJ,IF*P-1
 R*IFSEQ:13,VTM,IFCOM
 11,VJM,COM*M
 6,AEX,IFNAM
 ,U1A,REELSE1
 IF*P1:6,XTA,MISTABL
 6,AAX,CON72
 ,UZA,IF*P
 ,UJ,ERRIF
 R*IFSNE:13,VTM,IFCOM
 11,VJM,COM*M
 6,AEX,IFNAM
 ,UJ,R*IFSLT+2
 R*IFSGT:13,VTM,IFCOM
 11,VJM,COM*M
 6,XTS,IFNAM
 6,AEX,NGZB
 15,ARX,
 ,UJ,R*IFSLT+2
 R*IFSGE:13,VTM,IFCOM
 11,VJM,COM*M
 6,AEX,NGZB
 6,ARX,IFNAM
 ,U1A,REELSE1
 ,UJ,IF*P1
 R*IFSLT:13,VTM,IFCOM
 11,VJM,COM*M
 6,AEX,NGZB
 6,ARX,IFNAM
 ,U1A,IF*P1
 ,UJ,REELSE1
 R*IFSLE:13,VTM,IFCOM
 11,VJM,COM*M
 6,XTS,IFNAM
 6,AEX,NGZB
 15,ARX,
 ,UJ,R*IFSGE+2
 R*IFIS:13,VTM,IFCOM
 11,VJM,COM*M
 6,XTS,IFNAM
 13,VJM,IFIO
 ,UZA,IF*P1
 ,UJ,REELSE1
 R*IFOS:13,VTM,IFCOM
 11,VJM,COM*M
 6,XTS,IFNAM
 13,VJM,IFIO
 ,UZA,REELSE1
 ,UJ,IF*P1
 IFCOM:,ITA,
 6,ATX,IFNAM
 6,ATX,IFCNTM
 1,XTA,TFFF-PER
 1,ATX,14
 1,XTA,7
 1,ATX,13
 13,VJM,*RENA+2
 :8,VTM,
 13,VJM,*WRIN
 :6,XTA,IMIST
 ,UZA,IFCOM1
 :6,XTA,CON02
 ,UJ,ERRIF-1
 FFF:8,VTM,1
 13,VJM,*WRIN
 :6,XTA,IMIST
 ,UTC,
 ERRWS:13,VTM,FFF-1
 ,U1A,*READ4
 IFCOM1:6,XTA,POSIND
 7,VTM,-48
 14,VTM,-4
 ,UZA,GETSW+1
 :6,UTC,SSW+4
 14,AEX,
 ,UZA,GETSW
 6,XTA,POSIND
 14,VLM,*-2
 13,VJM,*READ4
 6,XTA,CON02
 :6,AOX,MISTABL
 6,ATX,MISTABL
 ERRIF:13,VTM,ABP1
 ,UJ,ERIFPR
 GETSW:6,UTC,MODEL+4
 14,XTA,
 :6,ATX,MESADR1
 11,UTC,
 8,VZM,
 13,VJM,READ
 :6,ATX,IFCNT
 14,UTM,-32
 14,VZM,READ
 14,UTM,-9
 6,AEX,R3B
 7,UTM,8
 14,VZM,ERRWS
 14,UTM,1
 ,UZA,OLDS
 14,VZM,ERRWS
  14,UTM,-6
 14,VZM,EDITTT*
 14,UTM,-81
 14,VZM,ENDALL
 6,XTA,IFCNT
 7,ASN,64
 6,AEX,IFNAM
 6,ATX,IFNAM
 7,V1M,READ
 4,UTM,1
 EDITTT*:4,UTM,-1
 13,VJM,*READ3
 ENDALL:6,XTA,MESADR1
 11,UJ,
 OLDS:6,XTA,RCON377
 7,ASN,64
 6,AAX,MESADR1
 6,AEX,IFNAM
 6,ATX,IFNAM
 ,UJ,READ
 IFIO:6,AAX,CON776
 6,ATX,IFCNT
 6,XTA,IFNAM
 ,ASN,64-8
  6,STX,IFNAM
 6,ATX,IFADRF
 ,UJ,IFIO2
 IFIO1:6,XTA,IFNAM
 ,ASN,64-8
 13,UZA,
 6,ATX,IFNAM
  6,AAX,CON776
 6,ATX,IFCNT
 6,XTA,MESADR1
 6,ATX,IFADRF
 IFIO2:6,AAX,CON776
 6,AEX,IFCNT
 ,UZA,IFIO1
 14,VTM,-4
 CCY:6,XTA,IFADRF
 ,ASN,64-8
 6,ATX,IFADRF
 6,AAX,CON776
 6,AEX,IFCNT
 ,UZA,IFIO1
 14,VLM,CCY
 13,UJ,
 R*IFEQ:13,VTM,COM*A
 11,VJM,COM*M
 :15,AOX,
 ,UJ,R*IFGE+2
 R*IFNE:13,VTM,COM*A
 11,VJM,COM*M
 :15,AOX,
 ,UJ,R*IFLT+2
 R*IFLE:13,VTM,COM*A
 11,VJM,COM*M
 :6,AVX,NGZB
 ,UJ,R*IFGE+1
 R*IFGE:13,VTM,COM*A
 11,VJM,COM*M
 :15,UTM,-1
 ,UTC,
 :,UZA,IF*P1
 ,UJ,REELSE1
 R*IFGT:13,VTM,COM*A
 11,VJM,COM*M
 :6,AVX,NGZB
 ,UJ,R*IFLT+1
 R*IFLT:13,VTM,COM*A
 11,VJM,COM*M
 :15,UTM,-1
 ,UTC,
 ,U1A,IF*P1
 ,UJ,REELSE1
 R*IFPL:13,VTM,COM*A1
 11,VJM,COM*M
 :,ASN,64+14
 ,UJ,R*IFGE+2
 R*IFMI:13,VTM,COM*A1
 11,VJM,COM*M
 :,ASN,64+14
 ,UJ,R*IFLT+2
 COM*A:,ITA,11
 13,VTM,*+2
 :15,ATX,
 ,UJ,PADRESS2
 :6,XTA,BLDEF
 15,WTC,
 11,VTM,
 ,U1A,*+4
 :15,XTA,3
 15,AEX,1
 15,XTS,3
 ,NTR,7
 15,A-X,1
 11,UJ,
 :13,VTM,ERRIF
 ,UJ,NODEFAD
 COM*A1:,ITA,11
 13,VJM,COM*A+1
 :6,ATX,MESADR5
 6,ATX,XADRESS5
 :6,XTA,BLDEF
 15,WTC,
 11,VTM,
 ,U1A,COM*A1-1
 6,XTA,MESADR1
 ,U1A,*+2
 :6,XTA,MESADR3
 11,UJ,
 :6,AEX,CON1
 ,UZA,*-1
 6,XTA,MESADR1
 6,AAX,CONBAS
 6,AEX,MESADR1
 ,U1A,*-3
 6,XTA,MESADR1
 ,ASN,64-15
 ,YTA,
 13,UJ,
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ LIST.
C*
C*
C*   YПPABЛEHИE ФOPMATOM ЛИCTИHГA
C*
C*   ЗHAKAMИ + - ПOMEЧEHO CTAHДAPTHOE
C*   ЗHЧEHИE ДAHHOЙ MOДЫ. ECЛИ MOДA
C*   YKAЗAHA B KAPTE *ASSEMBLER(L= ), EE
C*   CTAHДAPTHOE ЗHAЧEHИE ЗAMEHЯETCЯ HA
C*   OБPATHOE.
C*    B + BILIST
C*    C - LIST CONTROL COMMANDS
C*    E + EDIT LINE
C*    F - PRINT IF-SKIPED LINES
C*    G - GENERETED CODE
C*    L + LIST ON
C*    M - USER MACROS
C*    N + NO SUPPRESS
C*    P - PAGE PROPAGATION
C*    R - REFERENCE TABLE
C*    S - SYSTEM MACROS
C*    T + SYSTEM TEXT TABLE
C*    U - PRINT AUTO-UTC COMMAND
C*    V - DISPLAY LISTING
C*    W - WIDTH LIST
C*    X - PRINT X-TEXT LINES
C*
C*   ПPИ BЫЗOBE ACCEMБЛEPA L=0 OTMEHЯET
C*   ЛИCTИHГ, L YCTAHABЛИBAET YПPABЛEHИE
C*   ЛИCTИHГOM MOHИTOPHЫMИ KAPTAMИ.
C*
C
 R*LIST:,XTA,
 6,ATX,MSTR
 6,ATX,MSTR+1
 6,ATX,MSTR+2
 6,ATX,MSTR+3
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-53B
 14,VZM,L*PL1
 14,UTM,53B-55B
 14,VZM,L*MI1
 13,VJM,RELIST2
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B-53B
 14,VZM,L*PL
 14,UTM,53B-55B
 14,VZM,L*MI
 RELIST2:6,XTA,CN00004
 14,UTM,55B-52B
 14,VZM,L**
 14,UTM,52B-177B
 14,VZM,*+2
 14,ASN,177B-14B
 6,AAX,LISMASK0
 ,U1A,L*COM
 :6,XTA,MSTR+3
 ,UZA,L*ON
 ,ASN,64-1
 ,UZA,L*PL2
 ,ASN,64-1
 ,UZA,L*MI2
 12,VTM,STROKA+LISTECK
 12,XTA,
 6,ARX,RCON1
 ,ATI,11
 11,MTJ,13
 12,J+M,11
 13,UTM,-16
 13,VZM,*+2
 12,ATX,
 11,UTM,1
 :11,UTM,-1
 6,XTA,MSTR
 :6,AAX,LISMASK
 6,AOX,LISLAST
 6,XTS,MSTR+1
 6,AAX,LISMASK
 6,AEX,NGZB
 15,AAX,
 6,ATX,LISLAST
 11,ATX,
 RELISTE:14,VZM,*+1
 13,VJM,EDITTT
 :13,VTM,*ABP1
 ,UJ,LISTPR
 L**:12,VTM,STROKA+LISTECK
 12,XTA,
 ,ATI,14
 14,V1M,*+2
 :6,XTA,LISTAND
 ,UJ,*+4
 :14,UTM,-1
 ,ITA,14
 12,ATX,
 14,VZM,*-2
 12,J+M,14
 14,XTA,
 :15,ATX,1
 6,XTA,MSTR+3
 ,U1A,READ
 15,XTA,1
 6,ATX,LISLAST
 ,UJ,READ
 L*COM:6,ATX,MSTR+3
 6,AAX,LISMASK
 ,UZA,READ
 6,XTS,MSTR+2
 ,UZA,*+4
 6,XTA,MSTR+1
 15,AOX,-1
 6,STX,MSTR+1
 6,AAX,MSTR
 6,AEX,MSTR
 :6,ATX,MSTR
 ,UJ,READ
 :6,XTA,MSTR+1
 15,AAX,-1
 6,AEX,MSTR+1
 6,STX,MSTR+1
 6,AOX,MSTR
 ,UJ,*-3
 L*ON:13,VTM,STROKA+LISTECK
 13,XTA,
 ,ATI,11
 11,V1M,*+2
 6,XTA,LISTAND
 ,UJ,*+2
 :13,J+M,11
 11,XTA,
 :6,ATX,LISLAST
 ,UJ,RELISTE
 L*PL:6,ATX,MSTR+3
 ,XTA,
 :6,ATX,MSTR+2
 ,UJ,READ
 L*MI:6,ATX,MSTR+3
 ,UJ,L*PL+1
 L*MI1:6,XTA,CON2
 6,ATX,MSTR+2
 :6,ATX,MSTR+3
 ,UJ,READ
 L*PL1:6,XTA,CON4
 ,UJ,L*MI1+1
 L*MI2:6,XTA,LIS*L
 6,AAX,LISMASK
 6,AEX,NGZB
 6,AAX,LISLAST
 :6,ATX,LISLAST
 ,UJ,RELISTE
 L*PL2:6,XTA,LIS*L
 6,AAX,LISMASK
 6,AOX,LISLAST
 ,UJ,L*MI2+2
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ ERROR.
C*
C
 :11,VTM,ERRNE
 ,UJ,COM*ER
 :11,VTM,ERRGE
 ,UJ,COM*ER
 :11,VTM,ERRLT
 ,UJ,COM*ER
 :11,VTM,ERRGT
 ,UJ,COM*ER
 :11,VTM,ERRLE
 ,UJ,COM*ER
 :11,VTM,ERREQ
 ,UJ,COM*ER
 R*ERROR:6,XTA,MESMOD
 13,VTM,-5
 ,U1A,REERROR1
 6,XTA,MISTABL
 6,AAX,NCON7
 6,ATX,MISTABL
 GOTO:6,XTA,MESMET
 6,AEX,LOCTEXT
 ,U1A,*+2
 6,XTA,RCON1
 6,ATX,VERET
 ,UJ,*+5
 6,XTA,MISTABL
 6,AOX,CON4
 6,ATX,MISTABL
 6,XTA,CNR10000
 :6,AOX,FLGEX*
 6,ATX,FLGEX*
 13,VTM,REERRORE
 ,UJ,*READ3
 6,XTA,CON4
 ,UJ,*-2
 REERRORE:13,VTM,ABP1
 ,UJ,ERRORPR
 REERROR1:6,XTA,MESMOD
 6,UTC,LOCTEXT+6
 13,AEX,
 13,UZA,R*ERROR-1
 13,VLM,REERROR1
 13,VJM,*READ4
 6,XTA,CON1
 ,UTC,
 :6,AOX,MISTABL
 6,ATX,MISTABL
 REEREE:13,VTM,ABP1
 ,UJ,DECPRINT
 COM*ER:,ITA,11
 13,VJM,COM*A+1
 :15,WTC,
 11,VTM,
 ,UJ,COM*A+4
 ERREQ:15,AOX,
 ,UJ,ERRLT+1
 ERRNE:15,AOX,
 ,UJ,ERRGE+1
 ERRLE:6,AVX,NGZB
 ,UTC,
 ERRGE:15,UTM,-1
 ,UTC,
 :,UZA,GOTO
 ,UJ,REEREE
 ERRGT:6,AVX,NGZB
 ,UTC,
 ERRLT:15,UTM,-1
 ,UTC,
 :,U1A,GOTO
 ,UJ,REEREE
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ TITLE.
C*
C
 R*TITLE:6,XTA,MISTABL
 6,AAX,NCON76
 6,ATX,MISTABL
 6,XTA,TITLE2
 11,VTM,*CPRINT
 ,UZA,STORE
 6,XTA,FLAGTABL
 6,AAX,RCON1
 ,UZA,NOEJN
 11,VJM,STORE
 :6,XTA,LISLAST
 6,AAX,LIS*L
 ,UZA,*ABP1
 6,XTA,EJTEXT
 6,AEX,MESMET
 13,VTM,*+2
 ,UTC,
 ,UZA,EJECT
 :6,XTA,LISLAST
 6,AAX,LIS*C
 ,UZA,*ABP1
 ,UJ,*CPRINT
 EJOLD:11,VTM,EJOLD1
 ,UTC,INDTABL-TITLE2
 STORE:12,VTM,STROKA+TITLE2
 13,VJM,RELONG5
 :6,XTA,*PROBEL
 14,VTM,-9
 :12,UTC,9
 14,ATX,
 14,VLM,*-1
 6,AAX,AR
 12,ATX,10
 13,VJM,READ
 :14,UTM,-40B
 14,VZM,READ
 14,UTM,40B
 10,VTM,62
 7,VTM,-48
 13,VTM,*+1
 :6,AEX,RCON40
 7,ASN,64+8
 7,UTM,8
 14,UTM,-177B
 11,UTC,
 14,VZM,
 12,AEX,
 12,ATX,
 10,UTM,-1
 10,VZM,STOREE
 7,V1M,READ
 7,VTM,-48
 12,VLM,READ
 STOREE:11,MTJ,13
 ,UJ,*READ3
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ EJECT.
C*
C
 R*EJECT:6,XTA,MISTABL
 6,AAX,NCON3
 6,ATX,MISTABL
 13,VJM,CSKIP
 :13,VTM,EJOLD
 ,UZA,EJECT
 13,VTM,REEJECT1
 ,UJ,*READ3
 EJOLD1:6,XTA,INDTABL
 6,AEX,*PROBEL
 14,VTM,STROKA+INDTABL
 ,UZA,*+3
 ,ITS,14
 ,ITS,12
 6,XTS,RCON1
 13,VJM,PRINT8**
 :,ITA,
 6,ATX,INDTABL
 6,ATX,INDTABL+1
 ,UJ,*ABP1
 NOEJN:13,VTM,*ABP1
 ,UJ,*READ4
 CSKIP:6,XTA,FLAGTABL
 6,AAX,RCON1
 ,UZA,NOEJN
 6,XTA,LISLAST
 6,AAX,LIS*L
 ,UZA,NOEJN
 :6,XTA,LISLAST
 6,AAX,LIS*C
 6,ATX,IFCNT
 13,UJ,
 LEFCOMT:13,VTM,*ABP1A+6
 ,U1A,*READ4
 6,XTA,
 ,ASN,64-8
 6,ATX,
 6,XTA,10
 ,ASN,64+16
 6,ATX,10
 ,ITS,6
 14,VTM,STROKA+10
 ,ITS,14
 6,XTS,RCON1
 13,VTM,NOEJN
 ,UJ,PRINT8**
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ SPACE.
C*
C
 R*SPACE:6,XTA,MISTABL
 6,AAX,NCON2
 6,ATX,MISTABL
 13,VJM,CSKIP
 :6,XTA,MASK33
 6,ATX,MODMASK
 13,XTA,2
 6,ATX,*KOPPLG
 :,UJ,*RE8KOM
 ,UJ,*+1
 13,XTA,1
 1,ATX,14
 :13,Z31,PADRESSF
 ,UJ,SPAMES
 :6,XTS,*EQPLG
 6,STX,IFCNTM
 6,ATX,*EQPLG
 6,XTA,BLBSS
 ,UZA,*+2
 6,XTA,CON02
 6,AOX,MISTABL
 6,ATX,MISTABL
 :6,XTA,MISTABL
 6,AAX,CON72
 6,AEX,IFCNT
 13,VTM,*ABP1
 ,UZA,*+3
 6,XTA,IFCNTM
 ,UZA,*+1
 13,VJM,*READ5
 :13,VTM,*CPRINT
 ,UJ,SPACE
 :6,XTA,MESADR3
 6,ATX,AE
 6,XTA,IFCNTM
 ,UZA,SPACE
 6,XTA,RCON1
 6,ARX,MESADR3
 6,AAX,CONR15
 6,ATX,MESADR3
 13,VTM,EJOLD
 ,UJ,SPACE
 SPAMES:1,XTA,7
 6,ATX,*EQPLG
 1,WTC,97
 ,UJ,
 REEJECT1:13,VJM,EJECT
 *CPRINT:13,VTM,*ABP1
 ,UJ,CPRINT
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ LOC.
C*
C
 R*LOC:6,XTA,FLAGTABL
 6,AAX,RCON20
 ,UZA,*VMET6
 6,XTA,MISTABL
 6,AOX,CN0002
 6,ATX,MISTABL
 6,AAX,CN0004
 13,VTM,R*LOC1
 ,UZA,PADRESSF
 13,VJM,LEFCOM-1
 13,VJM,PADRESSF
 R*LOC1:6,XTA,NK
 13,VTM,ABP1
 :6,AEX,CONR15
 6,ARX,RCON1
 6,ARX,MESADR3
 6,AAX,CONR15
  6,ATX,SOS
 ,UJ,BSSPRINT
C
C**************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ D
C*
C
 R*D:6,XTA,FLAGTABL
 6,AAX,RCON20
 ,UZA,*VMET6
 13,VJM,*READ3
 6,XTA,MISTABL
 6,AAX,NCON76
 6,ATX,MISTABL
 6,XTA,FLAGTABL
 6,AAX,RCON1
 ,UZA,ABP1
 6,XTA,ASTNUMET
 14,VJM,HASHKOP+1
 :6,ATX,MESADR3
 6,XTA,SOS
 6,XTS,NK
  13,VJM,R*LOC1+1
 15,XTA,
 6,ATX,SOS
 ,UJ,ABP1
C
C***************************************
C*
C*   PUSH-DOWN STECK
C*
C*   BEЗДE HA 14 PEГ. - AДPEC CTEKA.
C*
C*   FORSTECK(ДЛИHA,ERROR) - ЗABOДИT CTEK
C*   BOЗBPAT: И15=И15-1, CYMMATOP=И14.
C*
C*   STECKIN(CЛOBO) - CЛOBO B CTEK.
C*   BOЗBPAT: AДP.CЛOBA - HA CYMMATOPE.
C*
C*   STECKOUT - CЛOBO ИЗ CTEKA.
C*   BOЗBPAT: CЛOBO - HA CYMMATOPE.
C*
C*   ПPИ OШИБKE (ECЛИ ERROR HE HYЛЬ):
C*   A) CTEK ПYCT (STECKOUT) ПEPEXOД K
C*      ERROR C HYЛEM HA CYMMATOPE;
C*   B) CTEK ПEPEПOЛHEH (STECKIN) - ПEPE-
C*      XOД K ERROR+1 CO CЛOBOM HA CYMMA-
C*      TOPE.
C*
C
 FORSTECK:,ASN,64-15
 15,AEX,
 ,ASN,64-15
 14,ATX,
 ,ITA,14
 13,UJ,
 STECKIN:14,XTS,
 ,ASN,64+15
 14,AEX,
 6,AAX,CONR15
 ,UZA,STECKERI
 14,XTA,
 6,ARX,RCON1
 14,STX,
 STCONT:14,WTC,
 14,ATX,
 ,ITA,14
 14,ARX,
 6,AAX,CONR15
 13,UJ,
 STECKOUT:14,XTA,
 6,AAX,CONR15
 ,UZA,STECKERO
 14,XTA,
 6,ARX,NG1B
 14,ATX,
 14,WTC,
 14,XTA,1
 13,UJ,
 STECKERI:14,XTA,
 ,ASN,64-3
 ,ASN,64-15
 ,YTA,
 ,UZA,*+2
 15,STX,1
 15,WTC,2
 ,UJ,1
 :15,XTA,
 ,UJ,STCONT
 STECKERO:14,XTA,
  ,ASN,64-3
  ,ASN,64-15
 ,YTA,
 13,UZA,
 ,XTS,
 15,WTC,
 ,UJ,
 MACSTECK:14,VTM,MSTACK-SCRACH
 ,ITA,14
 14,VTM,*+2
 ,ITS,14
 14,VTM,SCRACH
 ,UJ,FORSTECK
 :,MOD,
 :,UJ,FATNAME+4
C*
C
C***************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ  MACRO
C*
C*
C
C
C   PAБOЧИE  ЯЧEЙKИ
C
C   DPARAM - ПAPAMETPЫ MACRO
C   RE*RAB - CЧETЧИK ПAPAMETPOB
C   RE*RAB1- CЧETЧИK ПAPAMETPOB
C   RE*R   - ПPИЗHAK OPSYN/PURGE
C   MESADR1- AДPEC TEЛA MACRO
C   MESADR2- KOПИЯ NKCARD
C   MESADR3- ФЛAГ IRP
C   REISRR - METKA B KAPTE MAKPO
C   REISRR1- METKA B KAPTE ENDM
C   REISYM - MHEMOKOД B KAPTE
C   RR1,RR2,RR3,OCTAL
C
 R*MACRO:6,XTA,MISTABL
 6,AAX,CON6
 6,AEX,CON6
 ,UZA,*+2
 6,XTA,ASTNUMET
 ,U1A,*+2
 :6,ATX,REISRR1
 ,UJ,MERSET-1
 :6,ATX,REISRR1
 ,ASN,64-18
 ,YTA,
 ,U1A,MERSET-1
 6,XTA,REISRR1
 11,VTM,
 12,VTM,RMACRO1
 13,VJM,HASHKOP
 14,XTA,1
 ,UTC,ERRJAMP
 ,AEX,
 ,ASN,64+24
 12,UZA,
 6,XTA,RCON1
 6,ATX,EXTFLG
 6,XTA,MISTABL
 6,AOX,CON4
 6,STX,MISTABL
 13,VTM,FETCHER
 ,UJ,TWOCOP
 RMACRO1:,XTA,
 6,ATX,RR2
 6,ATX,DPARAM
 6,ATX,RE*RAB
 6,STX,RE*RAB1
 6,XTA,NKCARD
 6,ATX,MESADR2
 14,VTM,RMPARAM
 ,ITA,14
 1 ,ATX,14. ,
 1,XTA,/3/-PER
 1,ATX,13. +
 1,XTA,/4/-PER
 1,ATX,15. -
 14,VTM,RMPARAM*
 ,ITA,14
 1,ATX,97
 6,XTA,MESMOD
 8,VTM,ABP
 6,ATX,POSIND
 13,VTM,LOI
 12,VJM,RELONG5
 6,XTA,IMIST
 ,UZA,RMPARAM+2
 6,XTA,MISTABL
 6,AOX,CON1
 6,ATX,MISTABL
 8,UJ,
 RMPARAM:13,VTM,LOI
 12,VJM,*WRIN
 :6,XTA,IMIST
 ,U1A,MERSET
 :6,XTA,POSIND
 14,VTM,1-40
 ,U1A,RMPARAM1
 6,XTA,RE*RAB
 8,U1A,
 6,XTA,RE*RAB1
 6,ARX,RCON1
 6,ATX,POSIND
 RMPARAM1:14,UTC,39
 6,XTA,DPARAM
 ,UZA,WRPARAM
 6,AEX,POSIND
 ,U1A,*+2
 6,XTA,RE*RAB
 8,U1A,
 ,UJ,MERSET
 :14,VLM,RMPARAM1
 ,UJ,MERSET
 WRPARAM:6,XTA,POSIND
 14,UTC,39
 6,ATX,DPARAM
 ,XTA,
 14,UTC,40
 6,ATX,DPARAM
 6,XTA,RE*RAB1
 6,ARX,RCON1
 6,ATX,RE*RAB1
 14,VTM,-5
 CHECKPAR:14,UTC,5
 6,XTA,BADPARAM
 6,AEX,POSIND
 ,UZA,BADPAR*
 14,VLM,CHECKPAR
 8,UJ,
 BADPAR*:6,XTA,CON4
 6,AOX,MISTABL
 6,ATX,MISTABL
 6,XTA,POSIND
 8,MTJ,13
 ,UJ,BADPAR
 RMPARAM*:13,VTM,LOI
 12,VJM,*WRIN
 13,VJM,*READ3
 8,VTM,FETCHER
 13,VJM,RMPARAM+1
 6,XTA,RE*RAB
 ,U1A,RMPARAM2
 6,XTA,RE*RAB1
 6,ATX,RE*RAB
 RMPARAM2:14,VTM,R*LOCAL
 ,ITA,14
 6,ATX,IFPLG
 ,UJ,ABP1
 R*LOCAL:,XTA,
 6,ATX,IFPLG
 6,XTA,MISTABL
 6,AAX,CON74
 ,U1A,RLOCAL*-2
 6,XTA,MESKOP
 ,UTC,LOCAL*T
 ,AEX,
 ,U1A,RLOCAL*
 14,VTM,RMPARAM
 ,ITA,14
 1,ATX,14. ,
 14,VTM,RMPARAM*
 ,ITA,14
 1,ATX,97
 8,VTM,ABP
 8,MTJ,13
 ,UJ,RELONG5
 :6,XTA,MISTABL
 6,AAX,NCON76
 6,ATX,MISTABL
 RLOCAL*:,BSS,
 6,XTA,EXT
 6,ATX,MESADR1
 ,UTC,JMPMAC*
 ,XTA,
 12,VJM,EXTIN
 14,VTM,M*PARAM
 ,ITA,14
 1,ATX,9. '
 1,ATX,10. (
 1,ATX,11. )
 1,ATX,13. +
 1,ATX,15. -
 1,ATX,16. .
 1,ATX,MACSYM
 1,ATX,MACSYM1
 1,ATX,31. =
 14,VTM,FMNAME
 ,ITA,14
 1,ATX,28
 14,VTM,WRCARD
 ,ITA,14
 1,ATX,97
 14,VTM,MABP
 ,ITA,14
 6,ATX,AB*PLG
 ,XTA,
 6,ATX,INDTABL+1
 6,ATX,REISRR2
 6,ATX,MESADR3
 13,VTM,DMACROL1
 ,UJ,*READ4
 DMACROLP:1,XTS,C1READ1-PER
 13,VJM,READ*S
 :6,XTA,NKCARD
 6,ARX,RCON1
 6,ATX,NKCARD
 ,XTA,
 6,ATX,MESADR3
 6,ATX,MISTABL
 DMACROL1:8,VTM,ABP
 13,VJM,*READ4
 9,VTM,KART
 ,XTA,
 6,ATX,RR1
 6,XTA,*PROBEL
 6,ATX,RDFLAG
 13,VJM,READA
 13,VTM,DMACROLP
 14,UTM,-103B. C
 14,VZM,DECPRINT
 14,UTM,-52B+103B. *
 14,VZM,DECPRINT
 ,XTA,
 6,ATX,RR2
 6,ATX,RR3
 6,ATX,REISRR2
 6,XTA,EXT
 6,ATX,RE*R
 13,VJM,RELONG5
 13,VTM,MKOPARAM
 ,ITA,13
 1,ATX,14 . ,
 14,UTM,52B
 ,ITA,14
 ,UJ,ABP+1
 M*PARAM:13,VTM,LOI
 12,VJM,*WRIN
 :6,XTA,RCON377
 6,ATX,RR2
 :6,XTA,IMIST
 ,U1A,JLPM
 :6,XTA,POSIND
 ,UZA,JLPM
 6,XTA,INDTABL+1
 6,AOX,OCTAL
 ,U1A,JLPM
 14,VTM,-1
 :14,UTM,1
 14,UTC,
 6,XTA,DPARAM
 ,UZA,JLPM
 6,AEX,POSIND
 ,U1A,*-2
 ,ITA,14
 6,ARX,RCON320
 9,MTJ,14
 9,VTM,KART
 9,UTM,2
 9,ATX,-2
 14,XTA,-1
 9,ATX,-1
 ,UJ,JLPM
 MABP:,ITA,13
 6,XTS,INDTABL+1
 13,VTM,FORM
 ,UZA,READA+1
 6,STX,OCTAL
 ,UJ,JLPM1
 FORM:9,ATX,
 6,AEX,RCON40
 ,UZA,FORM1
 6,XTA,RR1
 9,UTM,1
 ,UZA,*+3
 ,ITS,14
 6,STX,RR1
 9,STX,-1
 ,UJ,M*PARAM
 :15,WTC,
 ,UJ,
 FORM1:6,XTA,RR1
 ,U1A,FORM1A
 6,XTA,RCON200
 6,ATX,RR1
 ,UJ,FORM1-1
 FORM1A:6,XTA,RR1
 6,ARX,RCON1
 6,ATX,RR1
 ,UJ,FORM1-1
 MKOPARAM:13,VTM,LOI
 12,VJM,*WRIN
 :12,XTA,2
 1,ATX,14. ,
 ,XTA,
 6,ATX,RR2
 :,UJ,M*PARAM+2
 ,UJ,*+1
 :1,XTA,MACSYM
 1,ATX,14.,
 6,XTA,RR2
 ,U1A,M*PARAM
 6,XTA,RCON377
 6,ATX,RR2
 6,XTA,INDTABL
 6,ATX,REISSYM
 13,VTM,LOI
 12,VJM,*WRIN
 *K*:6,XTA,IMIST
 6,AOX,OCTAL
 ,U1A,JLPM
 12,XTA,IRP*T-*K*
 6,AEX,REISSYM
 ,UZA,M*IRP
 13,VTM,END*T+1
 12,XTA,END*T-*K*
 6,AEX,REISSYM
 ,UZA,MENDM
 13,VTM,NAME*T+1
 12,XTA,NAME*T-*K*
 6,AEX,REISSYM
 ,UZA,MENDM
 13,VTM,FMNAME-2
 12,XTA,ENDM*T-*K*
 6,AEX,REISSYM
 ,U1A,M*PARAM+2
 6,XTA,REISRR2
 ,UZA,MENDM
 6,AEX,REISRR1
 ,U1A,M*PARAM+2
 MENDM:1,XTA,7
 1,ATX,10. (
 1,ATX,11. )
 1,ATX,16. .
 1,ATX,31 . =
 14,VTM,ABP
 ,ITA,14
 1,ATX,MACSYM
 1,ATX,MACSYM1
 ,XTA,
 6,ATX,AB*PLG
 6,XTA,FLAGTABL
 6,AAX,RCON1
 ,U1A,RMACRO2
 6,XTA,EXTFLG
 ,XTS,
 6,STX,EXTFLG
 13,U1A,
 ,NTR,3
 6,WTC,MESADR1
 ,XTA,
 6,XTS,RE*RAB1
 6,A-X,RE*RAB
 ,ASN,64-6
 15,AEX,
 6,AEX,RE*RAB
 6,WTC,MESADR1
 ,CTX,
 6,XTA,RE*R
 6,ATX,EXT
 :6,XTA,RCON1
 12,VJM,EXTIN
 RMACRO2:6,XTA,MESADR2
 13,UZA,
 6,XTA,REISRR1
 6,XTS,MESADR1
 ,ASN,64-24
 ,UJ,SETOPS
 :6,XTA,MACS5*
 6,ATX,RDFLAG
 13,VTM,*RENS9
 ,UJ,*READ4
 FMNAME:,BSS,
 6,XTA,INDTABL
 6,ATX,REISRR2
 13,VTM,LOI
 12,VJM,*WRIN
 6,XTA,IMIST
 6,AOX,OCTAL
 ,UZA,M*PARAM+1
 6,XTA,RCON1
 :6,ATX,REISRR2
 ,UJ,M*PARAM+1
 WRCARD:6,XTA,RCON12
 9,ATX,-1
 8,VJM,M*PARAM
 6,XTA,MACS5*
 6,ATX,RDFLAG
 13,VTM,DMACROLP
 12,VTM,DECPRINT
 6,XTA,RR3
 12,UZA,
 :,ASN,64-8
 10,VLM,*
 ,UJ,EXTIN
 M*IRP:6,XTA,RCON1
 9,ATX,
 9,UTM,1
 6,ATX,MESADR3
 JLPM:,XTA,
 6,ATX,OCTAL
 JLPM1:9,MTJ,11
 11,UTM,-KART
 11,VZM,LPM2
 9,VTM,KART
 6,XTA,MESADR3
 6,AEX,RCON1
 ,U1A,JLPM2
 6,XTA,RCON2
 6,ATX,MESADR3
 ,UJ,JLPM3
 JLPM2:6,AEX,RCON1
 ,UZA,JLPM3
 9,XTA,
 ,NTR,3
 6,A-X,RCON320
 ,U1A,IRPERR
 6,A-X,RE*RAB
 ,UZA,IRPERR1
 :,XTA,
 6,ATX,MESADR3
 JLPM3:,BSS,
 6,XTA,RR3
 ,U1A,LPM
 10,VTM,1-6
 ,XTA,
 LPM:,ASN,64-8
 9,XTS,
 15,AEX,
 9,UTM,1
 11,UTM,-1
 6,ATX,RR3
 11,VZM,LPM1
 10,VLM,LPM
 12,VTM,LPM-1
 ,UJ,EXTIN
 LPM1:9,VTM,KART
 10,VLM,LPM2
 ,XTS,
 6,STX,RR3
 12,VJM,EXTIN
 LPM2:,XTA,
 6,ATX,IMIST
 6,ATX,INDTABL+1
 6,XTA,RR1
 8,UZA,
 ,ATI,14
 9,ATX,
 9,UTM,1
 ,XTA,
 6,ATX,RR1
 8,UJ,+1
 IRPERR:9,XTA,
 ,ATI,14
 6,AAX,RCON200
 ,U1A,JLPM3
 14,UTM,-12B
 14,VZM,JLPM3-1
 14,UTM,-56B+12B
 14,VZM,JLPM3-1
 IRPERR1:6,XTA,CON02
 6,ATX,MISTABL
 6,XTA,RCON1
 6,ATX,EXTFLG
 6,XTA,MESADR1
 6,ATX,EXT
 ,XTA,
 6,ATX,MESADR2
 13,VTM,DMACROLP
 ,UJ,FETCHER-3
 IRPERR2:9,XTA,
 ,ATI,14
 :14,UTM,-12B
 14,VZM,JLPM3-1
 14,UTM,-56B+12B
 14,VZM,JLPM3-1
 ,UJ,IRPERR1
 IRPERR3:9,XTA,
 6,AAX,RCON200
 ,UZA,IRPERR1
 9,XTA,1
 ,ATI,14
 ,UJ,IRPERR2+1
 :6,XTA,CON2
 ,UJ,MERSET+1
 MERSET:6,XTA,CON02
 6,AOX,MISTABL
 6,ATX,MISTABL
 ,UJ,FETCHER
 :6,XTA,FLAGTABL
 6,AAX,RCON1
 13,UZA,
 6,XTA,MACS5*
 6,ATX,RDFLAG
 ,UJ,DECPRINT
 FETCHER:6,XTA,MISTABL
 ,ASN,64-5
 ,YTA,
 ,UZA,FETCHER-3
 SKIPMAC1:6,XTA,RCON1
 6,ATX,EXTFLG
 ,XTA,
 6,ATX,MESADR2
 ,UJ,RLOCAL*
 MACROPR:6,XTA,MACE5*
 6,ATX,RDFLAG
 13,VTM,*RENS9
 ,UJ,*READ4
C***************************************
C*
C*   OБPAБOTKA MAKPO BЫЗOBA
C*
C---------------------------------------
C    HA BXOДE: 13P-AДPEC TEЛA MAKPO
C              14P-AДPEC MAKPO B KOPTAB
C
C    ИHФOPMAЦИOHHOE ПOЛE MACROEX*
C
C    MRAC- CЧETЧИK BЛOЖEHHOCTИ
C    MACN  HOMEP KAPTЫ
C    ZMPAR - KOЛ-BO ПAPAMETPOB
C    LCNT,GCNT - CЧETЧИKИ ЛOKAЛOB
C    MWCNT - AДPEC TEKYЩEЙ KAPTЫ MAKPO
C    MARDIS - AДPEC ФAKTИЧ. ПAPAMETPOB
C
C    CTPYKTYPA CTEKA YПPЯTЫBAHИЯ MAKPO
C
C    WORD1/3/TYPE/2/LIS*MT/28/UNUSE/15/SA
C    WORD2/48/MACRO NAME
C    WORD3/12/LCNT/6/ZMPAR/15/MARDIS/15/MWCNT
C
C    TYPE - TИП (MACRO,MACROE,IRP,ECHO)
C    SA - STACK ADRESS
C
 MACROEX*:6,XTA,MISTABL
 6,AAX,CON7
 ,U1A,MACROPR
 ,ITA,13
 6,ATX,MESADR1
 13,XTA,-1
 6,AAX,AA6SIMBR
 6,ATX,RE*RAB
 6,ATX,RE*RAB1
 13,XTA,-1
 ,ASN,64+6
 6,AAX,AA6SIMBR
 6,ATX,MESADR3
 12,VTM,SCRACH
 12,XTA,
 6,ATX,MESADR2
 14,XTA,1
 6,AAX,CON005
 ,UZA,*+1
 6,XTA,LIS*MS
 :6,AEX,LIS*S
 6,AOX,MAINPRIN
 6,ATX,DPARAM
 ,XTA,
 6,ATX,RR2
 6,ATX,RR3
 14,VTM,MZZZ
 ,ITA,14
 6,ATX,AB*PLG
 11,VTM,1-6
 13,VJM,RELONG5
 6,XTA,MESMOD
 ,U1A,*+2
 6,ATX,AB*PLG
 ,UJ,ZZZP
 :9,VTM,-8
 15,ATX,
 MZZZ:15,XTA,
 ,ASN,64-6
 15,ATX,
 ,YTA,
 12,VTM,FORMZM
 9,VLM,MDL*6T8S
 ,XTA,
 6,STX,AB*PLG
 ,UJ,ZZZP
 ZZZL:14,MTJ,9
 9,UTM,-40B. BLANK
 9,VZM,READ
 9,UTM,-50B+40B. (
 9,VZM,ZZZ1
 9,UTM,-51B+50B
 9,VZM,ZZZ12-2
 ZZZC:14,UTM,-56B. .
 14,VZM,ZZZE
 14,UTM,-54B+56B. ,
 14,VZM,ZZZP
 14,UTM,-177B+54B
 14,VZM,ZZZE
 13,VTM,ZZZC
 ,UJ,FORMZM
 ZZZ1:6,XTA,RCON1
 6,ATX,RR1
 6,ATX,RR2
 13,VJM,READ
 ZZZ11:14,UTM,-50B.(
 13,VTM,ZZZ11
 14,VZM,ZZZ12
 14,UTM,-51B+50B.)
 14,VZM,ZZZ13
 14,UTM,-177B+51B
 14,VZM,ZZZ12-2
 ,UJ,FORMZM
 :14,VTM,SCRACH
 6,XTA,MESADR2
 14,ATX,
 ,UJ,MACROPR1
 ZZZ12:6,XTS,RR1
 6,ARX,RCON1
 6,STX,RR1
 ,UJ,FORMZM
 ZZZ13:6,XTA,RR1
 6,ARX,NG1B
 6,ATX,RR1
 ,UZA,ZZZP-1
 14,VTM,51B. )
 ,ITA,14
 ,UJ,FORMZM
  FORMZM:15,ATX,
 6,AEX,RCON40
 6,AOX,RR2
 ,U1A,*+2
 15,XTA,
 ,UJ,READ-1
 :6,XTA,RR3
 ,ASN,64-8
 15,AEX,
 6,ATX,RR3
 11,VLM,READ-1
 13,MTJ,11
 14,VTM,SCRACH
 13,VJM,STECKIN
 11,MTJ,13
 11,VTM,1-6
 ,UJ,READ-1
 :12,VTM,
 ,UJ,ZZZP+2
 ZZZP:13,VTM,ZZZL
 12,V1M,*+2
 12,VTM,1
 ,UJ,READ
 :6,XTA,RE*RAB
 6,ARX,NG1B
 6,ATX,RE*RAB
 6,AAX,CON4
 ,U1A,ZZZE1
 ,XTA,
 13,VTM,ZZZL
 6,ATX,RR2
 6,ATX,RR1
 ,UJ,FORMZM
 ZZZE:6,XTA,RE*RAB
 6,AAX,CON4
 ,U1A,ZZZE1
 6,XTA,RE*RAB
 6,ARX,NG1B
 6,ATX,RE*RAB
 6,XTA,RR3
 ,ASN,64-8
 6,ATX,RR3
 11,VLM,ZZZE
 14,VTM,SCRACH
 13,VJM,STECKIN
 11,VTM,1-6
 ,UJ,ZZZE
 ZZZE1:6,XTA,RR3
 ,ASN,64-8
 6,ATX,RR3
 11,VLM,ZZZE1
 14,VTM,SCRACH
 13,VJM,STECKIN
 :6,XTA,MESADR2
 6,AAX,CONR15
 6,XTS,MAINPRIN
 6,APX,LIS*MS
 ,ASN,64+3
 15,AEX,
 13,VJM,STECKIN
 6,XTA,MESKOP
 13,VJM,STECKIN
 6,XTA,LCNT
 ,ASN,64-6
 6,AEX,ZMPAR
 ,ASN,64-15
 6,AEX,MARDIS
 ,ASN,64-15
 6,AEX,MWCNT
 13,VJM,STECKIN
 ,ITA,14
 6,ARX,MESADR2
 6,ARX,RCON1
 6,AAX,CONR15
 6,ATX,MARDIS
 6,XTA,RE*RAB1
 6,ATX,ZMPAR
 6,XTA,MESADR1
 6,ATX,MWCNT
 6,XTA,MRAC
 6,ARX,RCON1
 6,ATX,MRAC
 6,XTA,GCNT
 6,ATX,LCNT
 6,ARX,MESADR3
 6,ATX,GCNT
 6,XTA,MISTABL
 6,AAX,CN0004
 :,UZA,*+1
 13,VJM,LEFCOM-1
 :6,XTA,MESKOP
 13,VJM,FORMNAME
 14,VTM,ZREAD*
 ,ITA,14
 6,ATX,MESREAD*
 12,VTM,MACEXPR
 ,UJ,ZUNPAR
 ZREAD*:,ATI,12
 6,XTA,CON4
 6,ATX,ZIRP
 ,ITA,9
 ,ITS,10
 ,ITS,11
 6,WTC,MWCNT
 ,XTS,
 14,VTM,
 11,VTM,1-6
 ZREAD1:6,AEX,RCON1
 ,UZA,ZENDM
 6,AEX,RCON1
 6,ATX,ZR1
 10,VTM,1-6
 ZREAD11:6,XTA,ZR1
 ,ASN,64-8
 6,ATX,ZR1
 ,YTA,
 6,ATX,ZR2
 6,AAX,RCON200
 ,U1A,ZREAD2
 6,XTA,ZR2
 6,AEX,RCON12
 ,UZA,ZREAD22
 6,XTA,ZR2
 6,AEX,RCON1
 ,UZA,ZREAD12
 14,UTC,
 12,XTA,
 ,ASN,64-8
 6,AEX,ZR2
 14,UTC,
 12,ATX,
 11,VLM,ZREAD12
 14,UTM,-11
 14,VZM,ZREAD3
 14,UTM,12
 11,VTM,1-6
 ZREAD12:10,VLM,ZREAD11
 6,XTA,MWCNT
 6,ARX,RCON1
 6,ATX,MWCNT
 6,WTC, MWCNT
 ,XTA,
 ,UJ,ZREAD1
 ZREAD2:6,XTA,ZR2
 ,ATI,9
 ,NTR,3
 6,A-X,RCON320
 ,UZA,ZREAD4
 9,UTM,-200B
 ZREAD21:6,XTA,RCON40
 14,UTC,
 12,XTS,
 ,ASN,64-8
 15,AEX,
 14,UTC,
 12,ATX,
 11,VLM,*+3
 14,UTM,-11
 14,VZM,ZREAD3
 14,UTM,12
 11,VTM,1-6
 :9,VZM,ZREAD12
 9,UTM,-1
 ,UJ,ZREAD21
 ZREAD22:9,VTM,100
 ,UJ,ZREAD21
 ZREAD3:6,WTC,MWCNT
 ,XTA,
 15,ATX,
 6,AAX,MSK*200
 6,ATX,ZR1
 15,AEX,
 6,AEX,MSK*012
 6,ARX,MSK*177
 6,AAX,MSK*200
 6,AOX,ZR1
 6,AEX,MSK*200
 ,U1A,ZREAD31
 6,XTA,MWCNT
 6,ARX,RCON1
 6,ATX,MWCNT
 ,UJ,ZREAD3
 ZREAD31:6,XTA,MACN
 12,ATX,12
 6,XTA,MACN+1
 12,STX,13
 ,STI,11
 ,STI,10
 ,ATI,9
 6,XTA,MWCNT
 6,ARX,RCON1
 6,STX,MWCNT
 13,UJ,
 ZREAD4:15,ATX,
 6,A-X,ZMPAR
 ,UZA,ZRLOC
 15,XTA,
 ,ATI,9
 9,UTC,DISCRIPT
 ,XTA,
 ,UZA,ZREAD12
 6,ATX,ZR3
 6,XTA,ZIRP
 6,AAX,CON4
 ,UZA,ZREAD40
 ,ITA,9
 6,AEX,CONR15
 6,ATX,ZIRP
 ZREAD40:,BSS,
 ,ITA,10
 6,WTC,ZR3
 ,XTS,
 15,ATX,
 10,VTM,1-6
 ZREAD42:15,XTA,
 ,ASN,64-8
 15,ATX,
 ,YTA,
 ,UZA,ZREAD43
 14,UTC,
 12,XTS,
 ,ASN,64-8
 15,AEX,
 14,UTC,
 12,ATX,
 11,VLM,*+3
 14,UTM,-11
 14,VZM,ZREAD44
 14,UTM,12
 11,VTM,1-6
 :10,VLM,ZREAD42
 6,XTA,ZR3
 6,ARX,RCON1
 6,ATX,ZR3
 10,VTM,1-6
 6,WTC,ZR3
 ,XTA,
 15,ATX,-1
 ,UJ,ZREAD42
 ZREAD43:15,XTA,
 15,XTA,
 ,ATI,10
 ,UJ,ZREAD12
 ZREAD44:15,UTM,-2
 ,UJ,ZREAD3
 ZRLOC:6,ARX,LCNT
 ,ASN,64-36
 6,AUX,ZGOST7
 6,AEX,ZISO0
 ,ASN,64+16
 ,ASN,64-32
 6,ATX,INDTABL+1
 ,YTA,
 6,AEX,*ЬЬЬЬ
 6,STX,INDTABL+2
 ,ITA,10
 10,VTM,STROKA+INDTABL
 ,ITS,10
 6,ATX,ZR3
 6,XTA,ZIRP
 6,AAX,NCON4
 6,ATX,ZIRP
 6,XTA,INDTABL+2
 15,ATX,
 10,VTM,1-6
 ,UJ,ZREAD42
 ZENDM:6,XTA,IRPF1
 ,U1A,R*IRP2-1
 14,VTM,SCRACH
 14,WTC,
 14,XTA,-1
 12,MTJ,13
 12,VJM,MDL*6T8W
 13,STX,1
 13,ATX,
 6,XTA,ENDM*S
 13,ATX,2
 6,XTA,ENDM*S+1
 13,ATX,3
 14,VTM,1-8
 6,XTA,*PROBEL
 :14,UTC,7
 13,ATX,4
 14,VLM,*-1
 13,MTJ,12
 14,VTM,SCRACH
 13,VJM,ZREAD3
 13,VJM,STECKOUT
 15,ATX,
 6,AAX,CONR15
 6,STX,MWCNT
 ,ASN,64+15
 15,ATX,
 6,AAX,CONR15
 6,STX,MARDIS
 ,ASN,64+15
 15,ATX,
 6,AAX,AA6SIMBR
 6,STX,ZMPAR
 ,ASN,64+6
 6,ATX,LCNT
 13,VJM,STECKOUT
 13,VJM,STECKOUT
 15,ATX,
 14,AEX,
 6,AAX,CONR15
 14,AEX,
 14,STX,
 ,ASN,64-3
 6,AUX,LIS*MS
 6,AEX,MAINPRIN
 6,AAX,LIS*MS
 6,AEX,MAINPRIN
 6,ATX,DPARAM
 6,XTA,MRAC
 6,ARX,NG1B
 ,U1A,*+2
 ,XTA,
 6,ATX,MESREAD*
 :6,ATX,MRAC
 ,UZA,ZENDM1
 12,VJM,ZUNPAR
 14,WTC,
 14,XTA,-1
 13,VJM,FORMNAME
 ZENDM1:,XTA,
 6,ATX,IFPLG
 6,ATX,SKIPFLAG
 14,VTM,MABP
 ,ITA,14
 6,AEX,AB*PLG
 13,VTM,MACEXPR
 13,U1A,
 6,XTA,EXT
 6,ATX,RE*R
 ,UJ,MENDM
 ZUNPAR:14,VTM,SCRACH
 14,XTA,
 6,ATX,MESADR1
 ,ITS,14
 15,ARX,
 6,ARX,RCON1
 6,AAX,CONR15
 6,ATX,MESADR2
 6,WTC,ZMPAR
 9,VTM,
 6,WTC,MARDIS
 11,VTM,
 8,VTM,DISCRIPT
 ,ITA,8
 6,ATX,MESADR3
 8,VTM,1-6
 ,XTA,
 6,ATX,RR3
 10,VTM,1-6
 :11,XTA,
 6,ATX,RR1
 ZUNP1:6,XTA,RR1
 ,ASN,64-8
 6,ATX,RR1
 ,YTA,
 ,UZA,ZUNP2
 6,XTS,RR2
 ,ASN,64-8
 15,AEX,
 6,ATX,RR2
 10,VLM,ZUNP3
 13,VJM,STECKIN
 6,ATX,RR3
 10,VTM,1-6
 ZUNP3:8,VLM,ZUNP1
 8,VTM,1-6
 11,UTM,1
 ,UJ,ZUNP1-1
 ZUNP21:6,XTA,RR3
 ,U1A,ZUNP2+1
 6,WTC,MESADR3
 ,ATX,
 ,UJ,ZUNP20
 ZUNP2:10,UTM,5
 10,VZM,ZUNP21
 :10,UTM,-5
 6,XTA,RR2
 :,ASN,64-8
 10,VLM,*
 13,VJM,STECKIN
 6,XTA,MESADR2
 6,WTC,MESADR3
 ,ATX,
 14,XTA,
 ,ITS,14
 15,ARX,
 6,ARX,RCON1
 6,AAX,CONR15
 6,ATX,MESADR2
 ZUNP20:6,XTA,MESADR3
 6,ARX,RCON1
 6,ATX,MESADR3
 10,VTM,1-6
 ,XTA,
 6,ATX,RR3
 9,UTM,-1
 9,V1M,ZUNP3
 ZUNP22:6,WTC,MESADR3
 8,VTM,
 ,NTR,3
 :,ITA,8
 ,ITS,14
 15,A-X,
 ,UZA,ZUNP23
 ,XTA,
 8,ATX,
 8,UTM,1
 ,UJ,ZUNP22+2
 ZUNP23:6,XTA,MESADR1
 14,ATX,
 12,UJ,
 MACEXPR:6,XTA,MACE5*
 6,ATX,RDFLAG
 13,VJM,DECPRINT
 6,XTA,DPARAM
 6,ATX,MAINPRIN
 MACEXOUT:13,VTM,ABP1
 ,UJ,*READ4
 FORMNAME:15,ATX,
 6,AAX,AA6SIMB
 ,U1A,*+2
 15,XTA,
 ,ASN,64-6
 ,UJ,FORMNAME
 :15,XTA,
 6,AAX,MASK30
 6,XTS,MRAC
 ,ASN,64-42
 6,AUX,MASK*07
 6,AEX,MASK*20
 ,ASN,64-12
 ,YTA,
 15,AEX,
 12,VJM,MDL*6T8W
 6,STX,MACN+1
 6,ATX,MACN
 13,UJ,
C
C
C***************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИЙ  I R P
C*
C
C
 R*IRP:6,XTA,MISTABL
 6,AAX,NCON7
 6,ATX,MISTABL
 6,XTA,LONGSTR
 6,ATX,NN
 6,XTA,MESREAD*
 ,UZA,MACROPR
 6,XTA,IRPF1
 6,AAX,CON4
 ,U1A,R*IRP2
 6,XTA,ZIRP
 6,AAX,CONR15
 ,UZA,SKIPIRP
 6,AEX,CONR15
 6,AOX,CON4
 6,ATX,IRPF1
 ,ATI,9
 9,UTC,DISCRIPT
 ,XTA,
 6,XTS,MWCNT
 ,ASN,64-15
 15,AEX,
 6,ATX,IRPF2
 ,ATI,12
 13,VTM,1-6
 RIRP29:10,VTM,1-6
 11,VTM,IRPF4+STROKA
 RIRP30:12,XTA,
 6,ATX,IRPF3
 :6,XTA,IRPF3
 ,ASN,64-8
 6,ATX,IRPF3
 ,YTA,
 ,ATI,14
 14,VZM,RIRP31
 14,UTM,-54B
 14,VZM,RIRP32
 14,UTM,-56B+54B
 14,VZM,RIRP31
 11,XTS,
 ,ASN,64-8
 15,AEX,
 11,ATX,
 10,VLM,RIRP34
 10,VTM,1-6
 11,UTM,1
 RIRP34: 13,VLM,RIRP30+1
 13,VTM,1-6
 12,UTM,1
 ,UJ,RIRP30
 RIRP31:6,XTA,IRPF1
 6,AOX,CON2
 6,ATX,IRPF1
 RIRP32:,ITA,12
 ,ASN,64-15
 ,ITS,13
 15,AEX,
 ,ASN,64-6
 6,XTS,IRPF1
 6,XTS,CON7
 6,AEX,AA6SIMBR
 15,AAX,
 15,AEX,
 6,ATX,IRPF1
 11,XTA,
 :,ASN,64-8
 10,VLM,*
 11,ATX,
 11,VTM,IRPF4+STROKA
 ,ITA,11
 9,UTC,DISCRIPT
 ,ATX,
 6,XTA,IRPF1
 6,AAX,CON1
 ,U1A,MACEXOUT
 ,UJ,MACROPR
 RIRP2:6,XTA,IRPF1
 6,AAX,AA6SIMBR
 ,ATI,9
 6,XTA,IRPF1
 ,ASN,64+6
 15,ATX,
 ,STI,13
 ,ASN,64+15
 ,ATI,12
 11,VTM,IRPF4+STROKA
 10,VTM,1-6
 ,UJ,RIRP34
 :15,UTM,-4
 R*IRP2:6,XTA,IRPF1
 6,AOX,CON1
 6,ATX,IRPF1
 6,AAX,CON2
 ,U1A,RIRP40
 6,XTA,IRPF2
 ,ASN,64+15
 6,AAX,CONR15
 6,ATX,MWCNT
 ,UJ,RIRP2
 RIRP40:6,XTA,IRPF1
 6,AAX,AA6SIMBR
 ,ATI,9
 6,XTA,IRPF2
 6,AAX,CONR15
 9,UTC,DISCRIPT
 ,ATX,
 ,XTA,
 6,ATX,IRPF1
 6,XTA,MESKOP
 ,UTC,IRP*T
 ,AEX,
 ,U1A,MACEXOUT
 ,UJ,MACROPR
 SKIPIRP:14,VTM,SKIPIRP1
 ,ITA,14
 6,ATX,IFPLG
 ,UJ,MACROPR
 SKIPIRP1:,XTA,
 6,ATX,MISTABL
 6,XTA,MESKOP
 ,UTC,IRP*T
 ,AEX,
 ,U1A,SKIPIRP2
 6,ATX,IRPF1
 6,ATX,IFPLG
 ,UJ,MACROPR
 SKIPIRP2:13,VTM,MACEXOUT
 ,UJ,SKIPPR
C
C***************************************
C*
C*   ПOИCK MHEMOKOДA B HASH TAБЛИЦE
C*
C*    O Б P A Щ E H И E :
C*   HA CYMMATOPE MHEMOKOД HOPMAЛ. B
C*   ПPABO B KOДE TEXT.
C*   HA 13ИP - AДPEC ДЛЯ HOPMAЛЬHOГO
C*   BOЗBPATA
C*   HA 12ИP - AДPEC BOЗBPATA, ECЛИ
C*   MHEMOKOД HE HAШЛИ B TAБЛИЦE.
C*
C*   BOЗBPAT: HA 14ИP - AДPEC MHEMOKOДA
C*   B HASH TAБЛИЦE (AБCOЛЮTHЫЙ)
C*   ECЛИ MHEMOKOД HE HAШЛИ, TO HA 14ИP
C*   AДPEC ПOCЛEДHEГO CЛOBA B ЦEПOЧKE
C*   CCЫЛOK.
C*   ECЛИ ПPИ OБPAЩEHИИ ИP11=0 И HAЙДE-
C*   TCЯ ПYCTOE ИMЯ B ЦEПИ OMOHИMИИ,
C*   AДPEC ПEPBOГO TAKOГO ИMEHИ ЗACЫЛA-
C*   ETCЯ B ИP11.        ПPИ BOЗBPATE:
C*   MHEMOKOД ЗACЛAH B MAГAЗИH,
C*   ИP15=ИP15+1, CYMMATOP=0B.
C*
C
 HASHKOP:15,ATX,
 14,VTM,*+5
 :6,AAX,CUTH
 ,ASN,64-8
 6,AEX,PACKH
 ,NTR,2
 6,A*X,HASH
 6,AAX,RCON377
 ,NTR,7
 14,UJ,
 :,ATI,14
 14,J+M,14
 14,UTM,KOPTAB
 14,XTA,
 11,VZM,*+5
 ,UZA,*+2
 :15,AEX,-1
 13,UZA,
 :14,XTA,1
 6,AAX,CONR15
 12,UZA,
 ,ATI,14
 14,XTA,
 11,V1M,*-3
 :,U1A,*-4
 14,MTJ,11
 ,UJ,*-4
C
C
C***************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИЙ  OPSYN
C*
C
C
 R*OPSYN:6,XTA,MISTABL
 6,AAX,CON7
 ,U1A,MACROPR
 6,XTA,ASTNUMET
 ,UZA,MACROPR2
 ,ASN,64-18
 ,YTA,
 ,U1A,MACROPR2
 6,XTA,ASTNUMET
 11,VTM,
 12,VTM,ROPSYN1
 13,VJM,HASHKOP
 14,XTA,1
 ,UTC,ERRJAMP
 ,AEX,
 ,ASN,64+24
 12,UZA,
 6,XTA,MISTABL
 6,AOX,CON4
 6,STX,MISTABL
 13,VTM,MACROPR
 ,UJ,TWOCOP
 ROPSYN1:1,XTA,/3/-PER
 1,ATX,13.+
 1,XTA,/4/-PER
 1,STX,15.-
 14,VTM,OPSYN1
 ,ITA,14
 1,ATX,97
 ,UJ,ABP
 OPSYN1:6,XTA,IMIST
 ,U1A,MACROPR1
 6,XTA,INDTABL
 ,UZA,MACROPR1
 :6,XTA,INDTABL
 6,ATX,MESKOP
 13,VTM,LOI
 12,VJM,*WRIN
 6,XTA,IMIST
 ,U1A,OPATR.ECЛИ HE ИMЯ
 6,XTA,MESKOP
 11,VTM,
 12,VTM,MACROPR1-2
 13,VJM,HASHKOP
 14,XTA,1
 6,ATX,MESADR1
 ,UZA,MACROPR1
 6,AAX,CN0005
 6,AEX,CN0005
 ,U1A,OPSYNEW
 6,XTA,MISTABL
 6,AOX,CON4
 6,STX,MISTABL
 13,VTM,MACROPR
 ,UJ,LOCKNM
 :15,XTA,
 ,UJ,*+2
 :,XTA,
 6,ATX,RMIST
 MACROPR1:6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 ,UJ,MACROPR
 MACROPR2:6,XTA,MISTABL
 6,AOX,CON6
 6,ATX,MISTABL
 ,UJ,MACROPR
 OPSYNEW:15,XTA,
 6,XTA,MESMOD
 ,UZA,OPSYNEW1
 6,XTA,MESADR1
 6,AAX,CN0004
 ,UZA,OPATR
 OPSYNEW1:6,XTA,ASTNUMET
 6,XTS,MESADR1
 13,VTM,MACROPR
 SETOPS:14,VTM,COP37
 14,AOX,
 14,AEX,1
 6,STX,RR2 . CNTRL WORD
 6,ATX,RR1 . NAME
 13,MTJ,8
 11,VTM,
 12,VTM,SETOPS1
 13,VJM,HASHKOP
 14,XTA,1
 ,UTC,ERRJAMP
 ,AEX,
 ,ASN,64+24
 12,UZA,
 15,XTA,
 8,UJ,
 SETOPS1:6,XTA,FLAGTABL
 6,AAX,RCON1
 ,U1A,SETOPS21
 15,XTA,
 14,AEX,
 ,UZA,SETOPS2
 11,V1M,SETOPS3
 6,XTA,EXT
 ,ATI,11
 14,ARX,1
 14,UTC,1
 ,CTX,
 6,XTA,RR1
 12,VJM,EXTIN
 ,UTC,ERRJAMP
 ,XTA,
 12,VTM,SETOPS4
 ,UJ,EXTINC
 SETOPS3:6,XTA,RR1
 11,ATX,
 ,UTC,ERRJAMP
 ,XTA,
 11,AEX,1
 6,AAX,CONR15
 ,UTC,ERRJAMP
 ,AEX,
 11,CTX,1
 SETOPS4:11,MTJ,14
 SETOPS2:14,XTA,
 6,AOX,CON4
 14,ATX,
 11,VTM,
 13,VTM,STROKA
 6,XTA,RR1
 12,VJM,HASHKOP
 11,V1M,SETOPS5
 14,XTA,1
 6,ARX,EXT
 14,UTC,1
 ,CTX,
 15,XTA,
 12,VJM,EXTIN
 6,XTA,RR2
 12,VJM,EXTINC
 6,XTA,RCON1
 6,ATX,RESETFL
 8,UJ,
 SETOPS5:15,XTA,
 11,ATX,
 11,XTA,1
 6,AEX,RR2
 6,AAX,CONR15
 6,AEX,RR2
 11,CTX,1
 6,XTA,RCON1
 6,ATX,RESETFL
 8,UJ,
 ERRJAMP:,Z31,*ABP1A
 ,Z00,
 SETOPS21:15,XTA,
 14,XTA,
 6,AOX,CON4
 14,ATX,
 8,UJ,
 COP37:,Z37,
 ,Z00,77777B
 :,Z06,
 ,Z00,77777B
 OPATR:6,XTA,NGZB
 ,ASN,64-3
 6,ATX,MODMASK
 ,UTC,*OPATRR
 ,XTA,
 6,ATX,*KOPPLG
 *OPATRR:,UJ,*RE8KOM
 ,UJ,OPATR1
 OPATR1:6,XTA,MISTABL
 6,AAX,CON1
 ,U1A,MACROPR
 6,XTA,MESMOD
 ,ASN,64+1
 ,ATI,14
 14,UTM,RELONG
 ,YTA,
 ,UZA,*+2
 14,XTA,
 ,ASN,64-24
 ,UJ,*+2
 :14,XTA,
 6,AAX,MASK24
 :6,ATX,MESMOD
 6,XTA,IMIST
 ,U1A,OPATRA
 6,XTA,MESADR1
 6,AAX,CONR24
 6,AEX,MESMOD
 6,ATX,MESADR1
 ,UJ,OPSYNEW1
 OPATRA:6,XTA,MESKOP
 13,VJM,PER6T8
 6,ATX,MESADR1
 ,ASN,64+7
 6,AOX,RMIST
 ,U1A,MACROPR1-1
 6,XTA,MESADR1
 ,ASN,64-15
 6,AEX,MESMOD
 6,ATX,MESADR1
 ,UJ,OPSYNEW1
 EXTIN:6,XTS,EXTFLG
 ,U1A,EXTRET
 6,XTA,EXT
 6,AEX,EXTMAX
 ,UZA,FATNAME+2
 6,XTA,RCON1
 6,ARX,EXT
 6,STX,EXT
 6,WTC,EXT
 ,ATX,-1
 12,UJ,
 EXTINC:6,XTS,EXTFLG
 ,U1A,EXTRET
 6,XTA,EXT
 6,AEX,EXTMAX
 ,UZA,FATNAME+2
 6,XTA,RCON1
 6,ARX,EXT
 6,STX,EXT
 6,WTC,EXT
 ,CTX,-1
 12,UJ,
 EXTRET:15,XTA,
 12,UJ,
C
C***************************************
C*
C*   OБPAБOTKA ИHCTPYKЦИИ  PURGE
C*
C*
C
 R*PURGE:6,XTA,MISTABL
 6,AAX,NCON7
 6,ATX,MISTABL
 1,XTA,/3/-PER
 1,ATX,13. +
 1,XTA,/4/-PER
 1,ATX,15. -
 1,XTA,*ENDMA90-PER
 1,ATX,14. ,
 1,XTA,PURPLC-PER
 1,ATX,97
 13,VTM,ABP
 ,UJ,RELONG5
 PURGEC:,UTC,MACROPR-ABP
 ,UTC,
 PURGE1:8,VTM,ABP
 6,XTA,INDTABL
 8,UZA,
 6,ATX,RR1
 13,VTM,LOI
 12,VJM,*WRIN
 6,XTA,IMIST
 ,UZA,PURGE2
 ,XTA,
 6,ATX,IMIST
 6,XTA,MISTABL
 6,AOX,CON02
 6,ATX,MISTABL
 8,UJ,
 PURGE2:6,XTA,RR1
 ,ASN,64-18
 ,YTA,
 8,U1A,
 6,XTA,RR1
 11,VTM,
 12,VTM,PURGE21
 13,VJM,HASHKOP
 14,XTA,1
 6,AAX,CN0005
 6,AEX,CN0005
 8,MTJ,13
 ,U1A,PURGE21
 6,XTA,MISTABL
 6,AOX,CON4
 6,STX,MISTABL
 ,UJ,LOCKNM
 PURGE21:15,XTA,
 6,ATX,RR1
 ,UTC,ERRJAMP
 ,XTS,
 6,ATX,RR2
 ,UJ,SETOPS1
C
C+  OБPAБOTKA KAPTЫ   STEXT
C
 R*STEXT:,XTA,
 6,ATX,IFPLG
 ,UTC,STEXT*T
 ,XTA,
 6,AEX,MESKOP
 ,U1A,POSKOP
 6,XTA,MISTABL
 6,AAX,CON7
 ,U1A,*RENS99
 6,XTA,FLAGTABL
 6,AAX,RCON1
 ,UZA,*RENS99
 6,XTA,MESMET
 ,U1A,RSTEXT1
 ,XTA,ENTRY*
 8,VTM,
 RSTEXT1:6,ATX,STEXTPNT
 13,VJM,RELONG5
 12,VTM,-5
 11,VTM,STEXTCOM+STROKA
 10,VTM,-5
 13,VJM,READA
 :14,UTM,-177B
 14,VZM,RSTEXT2
 14,UTM,-40B+177B
 14,V1M,RSTEXT0
 8,V1M,READA
 ,UTC,1
 RSTEXT0:,BSS,
 8,VTM,
 11,XTS,
 ,ASN,64-8
 15,AOX,
 11,ATX,
 12,VLM,READA
 12,VTM,-5
 11,UTM,1
 10,VLM,READA
 11,XTA,-1
 ,AOX,RCON377
 ,AEX,RCON377
 11,ATX,-1
 ,UJ,*RENS99
 RSTEXT2:11,XTA,
 ,UZA,*RENS99
 :,ASN,64-8
 12,VLM,*
 11,ATX,
 ,UJ,*RENS99
C
C+  CCЫЛKИ HA OCHOBHЫE ПPOЦECCЫ MADLEN
C
 :,,
 ,Z00,PNKOPTAB.-2
 JMPMAC*:13,Z31,MACROEX*
 ,Z00,.-1
 PROCTAB:,Z00,*ABP1A .   0
 ,Z00,R*MCNT       .   1
 ,Z00,R*IFOS       .   2
 ,Z00,R*ELSE       .   3
 ,Z00,R*DUP        .   4
 ,Z00,*RELON2      .   5
 ,Z00,RE8KOP       .   6
 ,Z00,R*ZL         .   7
 ,Z00,R*OCT        .   8
 ,Z00,R*Z          .   9
 ,Z00,R*PC         .  10
 ,Z00,R*LOC        .  11
 ,Z00,R*LP         .  12
 ,Z00,R*ENDIF      .  13
 ,Z00,R*IFSGE      .  14
 ,Z00,R*PS         .  15
 ,Z00,*RELON4      .  16
 ,Z00,*RELON       .  17
 ,Z00,R*PURGE      .  18
 ,Z00,R*SET        .  19
 ,Z00,*RELON3      .  20
 ,Z00,R*LC         .  21
 ,Z00,R*DATE       .  22
 ,Z00,R*ARRAY      .  23
 ,Z00,R*ERROR      .  24
 ,Z00,R*LS         .  25
 ,Z00,R*IFSLE      .  26
 ,Z00,R*ENTRY      .  27
 ,Z00,R*TIME       .  28
 ,Z00,R*ISO        .  29
 ,Z00,R*MIN        .  30
 ,Z00,R*P*P        .  31
 ,Z00,R*END        .  32
 ,Z00,R*BLOCK      .  33
 ,Z00,R*TITLE      .  34
 ,Z00,R*SUBP       .  35
 ,Z00,R*COS        .  36
 ,Z00,R*IRP        .  37
 ,Z00,R*EQU        .  38
 ,Z00,R*P/P        .  39
 ,Z00,R*USE        .  40
 ,Z00,R*CALL       .  41
 ,Z00,R*REAL       .  42
 ,Z00,R*REQU       .  43
 ,Z00,R*RMT        .  44
 ,Z00,R*SU         .  45
 ,Z00,R*VFD        .  46
 ,Z00,R*CNTRL      .  47
 ,Z00,R*LOG        .  48
 ,Z00,R*IFPL       .  49
 ,Z00,R*IFGT       .  50
 ,Z00,R*ECHO       .  51
 ,Z00,R*ATAN       .  52
 ,Z00,R*IF         .  53
 ,Z00,R*RELS       .  54
 ,Z00,R*MACRO      .  55
 ,Z00,R*OMIC       .  56
 ,Z00,R*SP         .  57
 ,Z00,R*IFIS       .  58
 ,Z00,R*WEQ        .  59
 ,Z00,R*SPACE      .  60
 ,Z00,R*IFLT       .  61
 ,Z00,R*SC         .  62
 ,Z00,R*XTEXT      .  63
 ,Z00,R*TEXT       .  64
 ,Z00,*VMET6       .  65
 ,Z00,R*SIN        .  66
 ,Z00,R*IFMI       .  67
 ,Z00,R*IFSGT      .  68
 ,Z00,R*SS         .  69
 ,Z00,R*IFC        .  70
 ,Z00,R*BAS        .  71
 ,Z00,R*VJM        .  72
 ,Z00,R*PU         .  73
 ,Z00,R*BSS        .  74
 ,Z00,R*IFSLT      .  75
 ,Z00,R*LU         .  76
 ,Z00,R*PP         .  77
 ,Z00,R*GOST       .  78
 ,Z00,R*DATPR      .  79
 ,Z00,R*6TEXT      .  80
 ,Z00,R*IFEQ       .  81
 ,Z00,R*INT        .  82
 ,Z00,R*OPSYN      .  83
 ,Z00,R*IFLE       .  84
 ,Z00,R*EDIT       .  85
 ,Z00,RELONG/      .  86
 ,Z00,R*IFSEQ      .  87
 ,Z00,R*MAX        .  88
 ,Z00,R*SST        .  89
 ,Z00,R*SSW        .  90
 ,Z00,R*HERE       .  91
 ,Z00,R*IFNE       .  92
 ,Z00,R*TEL        .  93
 ,Z00,R*D          .  94
 ,Z00,R*DATA       .  95
 ,Z00,R*EJECT      .  96
 ,Z00,R*IFSNE      .  97
 ,Z00,R*REL        .  98
 ,Z00,R*PUNCH      .  99
 ,Z00,R*8TEXT      . 100
 ,Z00,R*LABEL      . 101
 ,Z00,R*MICRO      . 102
 ,Z00,R*LIST       . 103
 ,Z00,R*RJ         . 104
 ,Z00,R*BASE       . 105
 ,Z00,R*DMIC       . 106
 ,Z00,R*IFGE       . 107
 ,Z00,R*EMC        . 108
 ,Z00,R*EPC        . 109
 ,Z00,R*ASC        . 110
 ,Z00,R*JIF        . 111
 ,Z00,R*EXP        . 112
 ,Z00,R*LN         . 113
 ,Z00,R*STIME      . 114
 ,Z00,R*SQRT       . 115
 ,Z00,R*ASIN       . 116
 ,LOG,.KOHEЦ TAБЛИЦЫ
C
C**************************************
C*
C*  PLUGS FOR MODIFICATION ASSEMBLER
C*  BEFOR PASS 2.
C*
C
 :15,XTA,-1 .PLG73
 13,VJM,COMPRINT
 :,UZA,PLG72+3 .PLG72
 8,VJM,ADR*R
 :1,ATX,11
 13,VTM,ENDREB
 :13,VJM,*MACAF    .PLG20
 :13,VTM,ABP1 .*RENS9
 ,UJ,DECPRINT
 :15,XTA,-1 .PLGKOP1
 13,VJM,COMPRINT
 :15,ATX, .PLGKOP
 13,VJM,COMPRINT
 :1,XTA,REALRET-PER.PLG22
 6,ATX,CODPLG
 :6,XTA,MISTABL.PLG19
 6,AOX,CON02
 :6,XTA,XADRESS1 .PLG18
 15,UTM,-1
 :6,ATX,XADRESS5   .PLG17
 ,UJ,PLG17+3
 :,U1A,PLG15+7 .PLG16
 6,ATX,XADRESS1
 :6,ATX,BLBSS      .PLG15
 14,VJM,BASFORM
 :14,VTM,*ETAP3Z1+3.PLG13
 ,UJ,KOPNORM
 :,U1A,PLG13+1     .PLG12
 14,VJM,STOLONG
 :,UJ,*ETAP3Z3     .PLG11
 :6,XTA,XADRESS2   .*ETAP3Z4
 ,UZA,*ETAP3Z2+1
 :,U1A,ENDAID-1.PLG10
 6,XTA,XADRESS1
 :13,VTM,*ETAP3C0+1.PLG9
 ,UJ,PASS2C
 :6,XTA,XADRESS1   .ENDADR6
 ,UZA,*ETAP3C4-1
 :,U1A,*ETAP3-3 .PLG8
 ,UJ,*ETAP3-1
 :,UZA,*MACB1 .PLG7
 ,ITA,13
 ,NTR,3.PLG6
 6,A-X,NKABS
 :6,ATX,MESADR4.*MADEC2
 6,XTA,NK
 :6,XTA,RCON1      .PLG5
 6,ATX,BLBSS
 :14,VTM,ISKOP     .PLG4
 ,UJ,POISKI
 :14,VTM,*IS*3     .PLG3
 ,U1A,*ISKADR
 :6,AAX,CON02.PLG2
 ,U1A,PLG2+2
 :6,XTS,UTCCONST .PLG*LEF
 13,VJM,COMPRI
 :6,XTA,LEFINST .PLG23
 ,UJ,WTOLIB
 :6,XTA,LONGSTR .PLG1
 6,ATX,NN
 PLAG2:6,XTA,NKKOS .ABP1
 6,AEX,NK
C
C
C
C***************************************
C*                                     *
C*   ACCOЦИATИBHЫЙ ПEPEKЛЮЧATEЛЬ       *
C*   ИHCTPYKЦИЙ.                       *
C*                                     *
C*   OTHOCИTEЛЬHЫЙ HAЧAЛЬHЫЙ AДPEC OП- *
C*   PEДEЛЯET MYЛЬTИПЛИKATИBHЫЙ HASH - *
C*   ПPOЦECC. BXOД B ПEPEKЛЮЧATEЛЬ COC-*
C*   TOИT ИЗ ДBYX CЛOB.                *
C*                                     *
C*   ПEPBOE CЛOBO:                     *
C*    ИMЯ ИHCTPYKЦИИ B KOДE TEXT, HOP- *
C*    MAЛИЗOBAHHOE BПPABO.             *
C*                                     *
C*   TPETЬE ПOЛYCЛOBO:                 *
C*    ,UJ,ADDRESS                      *
C*    ГДE ADDRESS - ИCTИHЫЙ AДPEC ПPO- *
C*    ЦEДYPЫ ИЛИ AДPEC ЦEПИ CИHOHИMИИ. *
C*    ДЛЯ ИHCTPYKЦИЙ, BBEДEHHЫX C ПOMO-*
C*    ЩЬЮ OПEPAЦИЙ MACRO ИЛИ OPSYN B   *
C*    ДAHHOЙ ПPOГPAMME ПOЛYCЛOBO ИMEET *
C*    BИД:    ,Z31,ADRESS              *
C*    KOД  ,Z34,ADRESS  OЗHAЧAET, ЧTO  *
C*    ECTЬ TPETЬE CЛOBO C ДOПOЛHИTEЛЬ- *
C*    HOЙ ИHФOPMAЦИEЙ.                 *
C*                                     *
C*   ЧETBEPTOE ПOЛYCЛOBO:              *
C*    БИT 24 = 0, ECЛИ ИHCTPYKЦИЯ ЯB-  *
C*    ЛЯETCЯ MAШИHHOЙ ИHCTPYKЦИEЙ. B   *
C*    ЭTOM CЛYЧAE B БИTAX 16-23 COДEP- *
C*    ЖИTCЯ KOП, B ПPOTИBHOM CЛYЧAE    *
C*    БИTЫ 16-23 COДEPЖAT ИHФOPMAЦИЮ   *
C*    ДЛЯ ПPOЦEДYP OБPAБOTKИ ПCEBДOИH- *
C*    CTPYKЦИЙ.                        *
C*    БИTЫ 1-15 COДEPЖAT AДPEC ЦEПИ    *
C*    OMOHИMИИ ИЛИ 0B. B ПOCЛEДHEM CЛY-*
C*    ЧAE ЦEПЬ OMOHИMИИ OTCYTCTBYET.   *
C*                                     *
C***************************************
C
 KOPTAB:  ,TEXT,8H    MCNT
          ,UJ,R*MCNT                      . 000
         8,  ,
          ,TEXT,8H    IFOS
           ,UJ,R*IFOS                     . 001
         8,  ,
 ELSE*T:  ,TEXT,8H    ELSE
          ,UJ,R*ELSE                      . 002
         10,Z00,EXTEND
          ,TEXT,8H     DUP
          ,UJ,R*DUP                       . 003
         8,  ,
          ,TEXT,8H     *77
          ,UJ,*RELON2                     . 004
         1,Z37,                           . 077B
          ,TEXT,8H     AVX
          ,UJ,RE8KOP                      . 005
          ,Z14,EXTEND+2                   . 014B
          ,TEXT,8H     Z25
          ,UJ,R*ZL                        . 006
         5,Z10,EXTEND+4                   . 250B
          ,TEXT,8H     OCT
          ,UJ,R*OCT                       . 007
         8,  ,
          ,TEXT,8H     STX
          ,UJ,RE8KOP                      . 010
          ,Z01,EXTEND+6                   . 001B
          ,TEXT,8H     *65
          ,UJ,*RELON2                     . 011
         1,Z25,                           . 065B
          ,TEXT,8H
          ,  ,                            . 012
          ,  ,
          ,TEXT,8H     Z13
          ,UJ,R*Z                         . 013
         2,Z30,                           . 130B
          ,TEXT,8H      PC
          ,UJ,R*PC                        . 014
         8,Z00,EXTEND+8
          ,TEXT,8H     *53
          ,UJ,*RELON2                     . 015
         1,Z13,                           . 053B
 LOC*T:   ,TEXT,8H     LOC
          ,UJ,R*LOC                       . 016
 10,,
          ,TEXT,8H     Z01
          ,UJ,R*Z                         . 017
          ,Z10,                           . 010B
          ,TEXT,8H
          ,  ,                            . 020
          ,  ,
          ,TEXT,8H      LP
          ,UJ,R*LP                        . 021
         8,  ,
          ,TEXT,8H
          ,  ,                            . 022
          ,  ,
          ,TEXT,8H
          ,  ,                            . 023
          ,  ,
 ENDD*T:  ,TEXT,8H    ENDD
          ,UJ,R*ENDIF
 10,,
          ,TEXT,8H     FUN
          ,UJ,*RELON2                     . 025
         1,Z10,                           . 050B
          ,TEXT,8H
          ,  ,                            . 026
          ,  ,
          ,TEXT,8H   PRINT
          ,UJ,*RELON2                     . 027
         1,Z24,EXTEND+70                       . 064B
          ,TEXT,8H
          ,  ,                            . 030
          ,  ,
          ,TEXT,8H   IFSGE
           ,UJ,R*IFSGE                    . 031
         8,Z00,EXTEND+90
          ,TEXT,8H      PS
          ,UJ,R*PS                        . 032
 8,Z00,EXTEND+100
          ,TEXT,8H
          ,  ,                            . 033
          ,  ,
          ,TEXT,8H     VTM
          ,UJ,*RELON4                     . 034
         5,Z00,EXTEND+10                  . 240B
          ,TEXT,8H     Z32
          ,UJ,R*ZL                             . 035
         6,Z20,                           . 320B
          ,TEXT,8H      IJ
          ,UJ,*RELON                      . 036
         6,Z20,EXTEND+12                  . 320B
          ,TEXT,8H     ARX
          ,UJ,RE8KOP                      . 037
          ,Z13,                           . 013B
          ,TEXT,8H     *72
          ,UJ,*RELON2                     . 040
         1,Z32,EXTEND+14                  . 072B
          ,TEXT,8H   PURGE
          ,UJ,R*PURGE                     . 041
         8,  ,
          ,TEXT,8H     Z20
          ,UJ,R*ZL                        . 042
         4,Z00,EXTEND+16                  . 200B
          ,TEXT,8H     SET
          ,UJ,R*SET                       . 043
         8,Z00,EXTEND+18
          ,TEXT,8H     *60
          ,UJ,*RELON3                     . 044
         1,Z20,                           . 060B
          ,TEXT,8H      LC
          ,UJ,R*LC                        . 045
         8,Z00,EXTEND+72
          ,TEXT,8H
          ,  ,                            . 046
          ,  ,
          ,TEXT,8H    DATE
          ,UJ,R*DATE                      . 047
         8,  ,
          ,TEXT,8H
          ,  ,                            . 050
          ,  ,
          ,TEXT,8H   ARRAY
          ,UJ,R*ARRAY                     . 051
         8,  ,
          ,TEXT,8H   ERROR
          ,UJ,R*ERROR                     . 052
         8,  ,
          ,TEXT,8H
          ,  ,                            . 053
          ,  ,
          ,TEXT,8H
          ,  ,                            . 054
          ,  ,
          ,TEXT,8H     Z16
          ,UJ,R*Z                         . 055
         3,Z20,                           . 160B
          ,TEXT,8H     ITA
          ,UJ,RE8KOP                      . 056
         1,Z02,EXTEND+20                  . 042B
          ,TEXT,8H
          ,  ,                            . 057
          ,  ,
          ,TEXT,8H     *56
          ,UJ,*RELON2                     . 060
         1,Z16,EXTEND+22                  . 056B
          ,TEXT,8H     AEX
          ,UJ,RE8KOP                      . 061
          ,Z12,                           . 012B
          ,TEXT,8H     Z04
          ,UJ,R*Z                         . 062
         1,Z00,                           . 040B
          ,TEXT,8H      LS
          ,UJ,R*LS                        . 063
         8,  ,
          ,TEXT,8H
          ,  ,                            . 064
          ,  ,
          ,TEXT,8H     XTS
          ,UJ,RE8KOP                      . 065
          ,Z03,                           . 003B
          ,TEXT,8H
          ,  ,                            . 066
          ,  ,
          ,TEXT,8H     X-A
          ,UJ,RE8KOP                      . 067
          ,Z06,                           . 006B
          ,TEXT,8H     ANX
          ,UJ,RE8KOP                      . 070
          ,Z23,                           . 023B
          ,TEXT,8H   IFSLE
          ,UJ,R*IFSLE                     . 071
         8,  ,
          ,TEXT,8H     V1M
          ,UJ,*RELON4                     . 072
         7,Z10,                           . 350B
          ,TEXT,8H   ENTRY
          ,UJ,R*ENTRY                     . 073
         8,  ,
          ,TEXT,8H    TIME
          ,UJ,R*TIME                      . 074
         8,  ,
          ,TEXT,8H     A/X
          ,UJ,RE8KOP                      . 075
          ,Z16,                           . 016B
          ,TEXT,8H     MTJ
          ,UJ,RE8KOP                      . 076
         1,Z04,                           . 044B
          ,TEXT,8H
          ,  ,                            . 077
          ,  ,
          ,TEXT,8H     Z35
          ,UJ,R*ZL                        . 100
         7,Z10,                           . 350B
          ,TEXT,8H     ISO
          ,UJ,R*ISO                       . 101
         8,  ,
          ,TEXT,8H     *75
          ,UJ,*RELON2                     . 102
         1,Z35,                           . 075B
          ,TEXT,8H     E-N
          ,UJ,RE8KOP                      . 103
          ,Z35,                           . 035B
          ,TEXT,8H     Z23
          ,UJ,R*ZL                        . 104
         4,Z30,EXTEND+24                  . 230B
          ,TEXT,8H     U1A
          ,UJ,*RELON                      . 105
         5,Z30,                           . 270B
          ,TEXT,8H     E+X
          ,UJ,RE8KOP                      . 106
          ,Z24,                           . 024B
          ,TEXT,8H     *63
          ,UJ,*RELON2                     . 107
         1,Z23,                           . 063B
          ,TEXT,8H     MIN
          ,UJ,R*MIN                       . 110
         8,  ,
          ,TEXT,8H     Z11
          ,UJ,R*Z                         . 111
         2,Z10,                           . 110B
          ,TEXT,8H     AAX
          ,UJ,RE8KOP                      . 112
          ,Z11,EXTEND+26                  . 011B
          ,TEXT,8H     *51
          ,UJ,*RELON2                     . 113
         1,Z11,                           . 051B
          ,TEXT,8H     P*P
          ,UJ,R*P*P                       . 114
         8,  ,
          ,TEXT,8H
          ,  ,                            . 115
          ,  ,
 END*T:   ,TEXT,8H     END
          ,UJ,R*END                       . 116
         10,Z00,EXTEND+98
          ,TEXT,8H     VLM
          ,UJ,*RELON4                     . 117
 7,Z30,EXTEND+102 . 370B
          ,TEXT,8H   BLOCK
          ,UJ,R*BLOCK                     . 120
         8,  ,
          ,TEXT,8H
          ,  ,                            . 121
          ,  ,
          ,TEXT,8H
          ,  ,                            . 122
          ,  ,
          ,TEXT,8H     LIB
          ,UJ,*RELON3                     . 123
         1,Z26,EXTEND+92                  . 066B
          ,TEXT,8H   TITLE
          ,UJ,R*TITLE                     . 124
         8,  ,
          ,TEXT,8H     Z07
          ,UJ,R*Z                         . 125
         1,Z30,                           . 070B
          ,TEXT,8H
          ,  ,                            . 126
          ,  ,
          ,TEXT,8H
          ,  ,                            . 127
          ,  ,
          ,TEXT,8H     ASX
          ,UJ,RE8KOP                      . 130
          ,Z26,                           . 026B
          ,TEXT,8H
          ,  ,                            . 131
          ,  ,
          ,TEXT,8H     STI
          ,UJ,RE8KOP                      . 132
         1,Z01,                           . 041B
          ,TEXT,8H     Z30
          ,UJ,R*ZL                        . 133
 6,Z00,EXTEND+104 . 300B
          ,TEXT,8H
          ,  ,                            . 134
          ,  ,
          ,TEXT,8H
          ,  ,                            . 135
          ,  ,
          ,TEXT,8H     *70
          ,UJ,*RELON2                     . 136
         1,Z30,                           . 070B
 SUBP*T:  ,TEXT,8H    SUBP
          ,UJ,R*SUBP                      . 137
         10,Z00,EXTEND+28
          ,TEXT,8H
          ,  ,                            . 140
          ,  ,
 ,TEXT,8H     COS
 ,UJ,R*COS . 141
 8,,
 ENDIF*T: ,TEXT,8H   ENDIF
          ,UJ,R*ENDIF
 10,,
          ,TEXT,8H
          ,  ,                            . 143
          ,  ,
          ,TEXT,8H     A-X
          ,UJ,RE8KOP                      . 144
          ,Z05,                           . 005B
          ,TEXT,8H     XTA
          ,UJ,RE8KOP                      . 145
          ,Z10,EXTEND+94                  . 010B
          ,TEXT,8H     CTX
          ,UJ,*RELON2                     . 146
         1,Z35,                           . 075B
          ,TEXT,8H     Z26
          ,UJ,R*ZL                        . 147
         5,Z20,                           . 260B
 IRP*T:,TEXT,8H     IRP
          ,UJ,R*IRP                       . 150
 10,,
          ,TEXT,8H
          ,  ,                            . 151
          ,  ,
          ,TEXT,8H     *66
          ,UJ,*RELON3                     . 152
         1,Z26,                           . 066B
          ,TEXT,8H     EQU
          ,UJ,R*EQU                       . 153
         8,  ,
          ,TEXT,8H     RTE
          ,UJ,RE8KOP                      . 154
          ,Z30,EXTEND+30                  . 030B
          ,TEXT,8H     P/P
          ,UJ,R*P/P                       . 155
         8,Z00,EXTEND+32
          ,TEXT,8H     *54
          ,UJ,*RELON2                     . 156
         1,Z14,                           . 054B
          ,TEXT,8H
          ,  ,                            . 157
          ,  ,
          ,TEXT,8H     Z02
          ,UJ,R*Z                         . 160
          ,Z20,                           . 020B
          ,TEXT,8H     USE
          ,UJ,R*USE                       . 161
         8,  ,
          ,TEXT,8H     AOX
          ,UJ,RE8KOP                      . 162
          ,Z15,EXTEND+34                  . 015B
          ,TEXT,8H    DRUM
          ,UJ,*RELON2                     . 163
         1,Z30,                           . 070B
          ,TEXT,8H
          ,  ,                            . 164
          ,  ,
          ,TEXT,8H
          ,  ,                            . 165
          ,  ,
          ,TEXT,8H     VZM
          ,UJ,*RELON4                     .166
         7,Z00,EXTEND+36                  . 340B
          ,TEXT,8H
          ,  ,                            . 167
          ,  ,
          ,TEXT,8H
          ,  ,                            . 170
          ,  ,
          ,TEXT,8H
          ,  ,                            . 171
          ,  ,
          ,TEXT,8H    CALL
          ,UJ,R*CALL                      . 172
         6,Z10,                           . 310B
          ,TEXT,8H    REAL
          ,UJ,R*REAL                      . 173
         8,Z00,EXTEND+38
          ,TEXT,8H    DISC
          ,UJ,*RELON2                     . 174
         1,Z30,EXTEND+40                  . 070B
          ,TEXT,8H     E+N
          ,UJ,RE8KOP                      . 175
          ,Z34,                           . 034B
          ,TEXT,8H    REQU
          ,UJ,R*REQU                      . 176
         8,Z00,EXTEND+88
          ,TEXT,8H
          ,  ,                            . 177
          ,  ,
          ,TEXT,8H
          ,  ,                            . 200
          ,  ,
          ,TEXT,8H     UZA
          ,UJ,*RELON                      . 201
         5,Z20,EXTEND+42                  . 260B
          ,TEXT,8H
          ,  ,                            . 202
          ,  ,
          ,TEXT,8H     Z21
          ,UJ,R*ZL                        . 203
         4,Z10,                           . 210B
          ,TEXT,8H
          ,  ,                            . 204
          ,  ,
          ,TEXT,8H     *61
          ,UJ,*RELON2                     . 205
         1,Z21,                           . 061B
          ,TEXT,8H     RMT
          ,UJ,R*RMT                       . 206
         8,Z00,EXTEND+46
          ,TEXT,8H
          ,  ,                            . 207
          ,  ,
          ,TEXT,8H
          ,  ,                            . 210
          ,  ,
          ,TEXT,8H      SU
          ,UJ,R*SU                        . 211
         8,  ,
          ,TEXT,8H
          ,  ,                            . 212
          ,  ,
          ,TEXT,8H
          ,  ,                            . 213
          ,  ,
          ,TEXT,8H
          ,  ,                            . 214
          ,  ,
          ,TEXT,8H     VFD
          ,UJ,R*VFD                       . 215
         8,  ,
          ,TEXT,8H     Z17
          ,UJ,R*Z                         . 216
         3,Z30,                           . 170B
          ,TEXT,8H     ASN
          ,UJ,RE8KOP                      . 217
          ,Z36,                           . 036B
          ,TEXT,8H   CNTRL
          ,UJ,R*CNTRL                     . 220
         8,  ,
          ,TEXT,8H     LOG
          ,UJ,R*LOG                       . 221
         8,Z00,EXTEND+62
          ,TEXT,8H     ATX
          ,UJ,RE8KOP                      . 222
          ,Z00,                           . 000B
          ,TEXT,8H     Z05
          ,UJ,R*Z                         . 223
         1,Z10,                           . 050B
          ,TEXT,8H    IFPL
          ,UJ,R*IFPL                      . 224
         8,  ,
          ,TEXT,8H    IFGT
          ,UJ,R*IFGT                      . 225
         8,Z00,EXTEND+64
          ,TEXT,8H    ECHO
          ,UJ,R*ECHO                      . 226
         8,  ,
 ,TEXT,8H    ATAN
 ,UJ,R*ATAN . 227
 8,,
          ,TEXT,8H    TAPE
          ,UJ,*RELON2                     . 230
         1,Z30,                           . 070B
          ,TEXT,8H
          ,  ,                            . 231
          ,  ,
          ,TEXT,8H
          ,  ,                            . 232
          ,  ,
          ,TEXT,8H      IF
          ,UJ,R*IF                        . 233
         8,  ,
          ,TEXT,8H    RELS
          ,UJ,R*RELS                      . 234
         8,  ,
          ,TEXT,8H     EXT
          ,UJ,RE8KOP                      . 235
          ,Z33,EXTEND+48                  . 033B
          ,TEXT,8H     A+X
          ,UJ,RE8KOP                      . 236
          ,Z04,                           . 004B
          ,TEXT,8H   MACRO
          ,UJ,R*MACRO                     . 237
         8,  ,
          ,TEXT,8H    OMIC
          ,UJ,R*OMIC                      . 240
         8,  ,
          ,TEXT,8H     Z36
          ,UJ,R*ZL                        . 241
         7,Z20,                           . 360B
          ,TEXT,8H     VIM
          ,UJ,*RELON4                     . 242
         7,Z10,                           . 350B
          ,TEXT,8H     *76
          ,UJ,*RELON2                     . 243
         1,Z36,EXTEND+50                  . 076B
          ,TEXT,8H      SP
          ,UJ,R*SP                        . 244
         8,  ,
          ,TEXT,8H     Z24
          ,UJ,R*ZL                        . 245
         5,Z00,                           . 240B
          ,TEXT,8H
          ,  ,                            . 246
          ,  ,
          ,TEXT,8H    IFIS
          ,UJ,R*IFIS                      . 247
         8,  ,
          ,TEXT,8H     *64
          ,UJ,*RELON2                     . 250
         1,Z24,                           . 064B
          ,TEXT,8H     WEQ
          ,UJ,R*WEQ                       . 251
         8,Z00,EXTEND+52
          ,TEXT,8H     Z12
          ,UJ,R*Z                         . 252
         2,Z20,                           . 120B
          ,TEXT,8H     APX
          ,UJ,RE8KOP                      . 253
          ,Z20,                           . 020B
          ,TEXT,8H     *52
          ,UJ,*RELON2                     . 254
         1,Z12,EXTEND+54                  . 052B
          ,TEXT,8H     UIA
          ,UJ,*RELON                      . 255
         5,Z30,                           . 270B
          ,TEXT,8H     Z00
          ,UJ,R*Z                         . 256
          ,Z00,                           . 000B
          ,TEXT,8H
          ,  ,                            . 257
          ,  ,
          ,TEXT,8H     NTR
          ,UJ,RE8KOP                      . 260
          ,Z37,                           . 037B
          ,TEXT,8H
          ,  ,                            . 261
          ,  ,
          ,TEXT,8H     UTM
          ,UJ,*RELON4                     . 262
         5,Z10,EXTEND+56                  . 250B
          ,TEXT,8H   SPACE
          ,UJ,R*SPACE                     . 263
         8,  ,
          ,TEXT,8H
          ,  ,                            . 264
          ,  ,
          ,TEXT,8H    IFLT
          ,UJ,R*IFLT                      . 265
         8,Z00,EXTEND+58
          ,TEXT,8H
          ,  ,                            . 266
          ,  ,
          ,TEXT,8H
          ,  ,                            . 267
          ,  ,
          ,TEXT,8H
          ,  ,                            . 270
          ,  ,
          ,TEXT,8H      SC
          ,UJ,R*SC                        . 271
         8,  ,
          ,TEXT,8H   XTEXT
           ,UJ,R*XTEXT                    . 272
         8,  ,
          ,TEXT,8H    TEXT
          ,UJ,R*TEXT                      . 273
         8,Z00,EXTEND+60
          ,TEXT,8H     Z31
          ,UJ,R*ZL                        . 274
         6,Z10,                           . 310B
          ,TEXT,8H     WTC
          ,UJ,*RELON                      . 275
         4,Z30,                           . 230B
          ,TEXT,8H     ACX
          ,UJ,RE8KOP                      . 276
          ,Z22,                           . 022B
          ,TEXT,8H     *71
          ,UJ,*RELON2                     . 277
         1,Z31,                           . 071B
          ,TEXT,8H
          ,  ,                            . 300
          ,  ,
          ,TEXT,8H
          ,  ,                            . 301
          ,  ,
 CONT*T:  ,TEXT,8H    CONT
          ,UJ,*VMET6                      . 302
 10,,
          ,TEXT,8H
          ,  ,                            . 303
          ,  ,
 ,TEXT,8H     SIN
 ,UJ,R*SIN . 304
 8,,
          ,TEXT,8H    IFMI
          ,UJ,R*IFMI                      . 305
         8,Z00,EXTEND+66
          ,TEXT,8H   IFSGT
          ,UJ,R*IFSGT                     . 306
         8,  ,
          ,TEXT,8H      SS
          ,UJ,R*SS                        . 307
         8,  ,
          ,TEXT,8H     Z27
          ,UJ,R*ZL                        . 310
         5,Z30,                           . 270B
          ,TEXT,8H     IFC
          ,UJ,R*IFC                       . 311
         8,  ,
 NAME*T:  ,TEXT,8H    NAME
          ,UJ,*VMET6                      . 312
 10,,
          ,TEXT,8H     *67
          ,UJ,*RELON2                     . 313
         1,Z27,EXTEND+68                  . 067B
          ,TEXT,8H     AUX
          ,UJ,RE8KOP                      . 314
          ,Z21,                           . 021B
          ,TEXT,8H     Z15
          ,UJ,R*Z                         . 315
         3,Z10,EXTEND+74                  . 150B
          ,TEXT,8H    SKIP
          ,UJ,R*ELSE                      . 316
         8,  ,
          ,TEXT,8H     YTA
          ,UJ,RE8KOP                      . 317
          ,Z31,EXTEND+76                  . 031B
          ,TEXT,8H     BAS
          ,UJ,R*BAS                       . 320
         8,  ,
          ,TEXT,8H     Z03
          ,UJ,R*Z                         . 321
          ,Z30,                           . 030B
          ,TEXT,8H      UJ
          ,UJ,*RELON                      . 322
         6,Z00,                           . 300B
          ,TEXT,8H
          ,  ,                            . 323
          ,  ,
          ,TEXT,8H
          ,  ,                            . 324
          ,  ,
          ,TEXT,8H     XTR
          ,UJ,RE8KOP                      . 325
 ,Z27,EXTEND+106 . 027B
          ,TEXT,8H
          ,  ,                            . 326
          ,  ,
          ,TEXT,8H
          ,  ,                            . 327
          ,  ,
          ,TEXT,8H
          ,  ,                            . 330
          ,  ,
          ,TEXT,8H     MOD
          ,UJ,RE8KOP                      . 331
          ,Z02,                           . 002B
 STEXT*T: ,TEXT,8H   STEXT
          ,UJ,*VMET6                      . 332
 10,,
          ,TEXT,8H     VJM
          ,UJ,R*VJM                       . 333
         6,Z10,                           . 310B
          ,TEXT,8H      PU
          ,UJ,R*PU                        . 334
         8,  ,
          ,TEXT,8H
          ,  ,                            . 335
          ,  ,
          ,TEXT,8H     BSS
          ,UJ,R*BSS                       . 336
         8,  ,
          ,TEXT,8H     Z34
          ,UJ,R*ZL                        . 337
         7,Z00,                           . 340B
          ,TEXT,8H
          ,  ,                            . 340
          ,  ,
          ,TEXT,8H
          ,  ,                            . 341
          ,  ,
          ,TEXT,8H     *74
          ,UJ,*RELON2                     . 342
         1,Z34,EXTEND+78                  . 074B
          ,TEXT,8H
          ,  ,                            . 343
          ,  ,
          ,TEXT,8H     ATI
          ,UJ,RE8KOP                      . 344
         1,Z00,EXTEND+80                  . 040B
          ,TEXT,8H
          ,  ,                            . 345
          ,  ,
          ,TEXT,8H     *62
          ,UJ,*RELON2                     . 346
         1,Z22,                           . 062B
          ,TEXT,8H   IFSLT
          ,UJ,R*IFSLT                     . 347
         8,Z00,EXTEND+84
          ,TEXT,8H     Z10
          ,UJ,R*Z                         . 350
         2,Z00,                           . 100B
          ,TEXT,8H     UTC
          ,UJ,*RELON                      . 351
         4,Z20,                           . 220B
          ,TEXT,8H
          ,  ,                            . 352
          ,  ,
          ,TEXT,8H     *50
          ,UJ,*RELON2                     . 353
         1,Z10,                           . 050B
          ,TEXT,8H
          ,  ,                            . 354
          ,  ,
          ,TEXT,8H
          ,  ,                            . 355
          ,  ,
          ,TEXT,8H
          ,  ,                            . 356
          ,  ,
          ,TEXT,8H
          ,  ,                            . 357
          ,  ,
          ,TEXT,8H     NIL
          ,UJ,R*ENDIF                     . 360
         8,  ,
          ,TEXT,8H
          ,  ,                            . 361
          ,  ,
          ,TEXT,8H
          ,  ,                            . 362
          ,  ,
          ,TEXT,8H
          ,  ,                            . 363
          ,  ,
          ,TEXT,8H     Z06
          ,UJ,R*Z                         . 364
         1,Z20,                           . 060B
          ,TEXT,8H      LU
          ,UJ,R*LU                        . 365
         8,  ,
          ,TEXT,8H
          ,  ,                            . 366
          ,  ,
          ,TEXT,8H      PP
          ,UJ,R*PP                        . 367
         8,  ,
          ,TEXT,8H    GOST
          ,UJ,R*GOST                      . 370
         8,  ,
          ,TEXT,8H
          ,  ,                            . 371
          ,  ,
          ,TEXT,8H   DATPR
          ,UJ,R*DATPR                     . 372
 8,Z00,EXTEND+108
          ,TEXT,8H   6TEXT
          ,UJ,R*6TEXT                     . 373
         8,  ,
          ,TEXT,8H
          ,  ,                            . 374
          ,  ,
          ,TEXT,8H
          ,  ,                            . 375
          ,  ,
          ,TEXT,8H     ITS
          ,UJ,RE8KOP                      . 376
         1,Z03,EXTEND+86                  . 043B
          ,TEXT,8H    IFEQ
          ,UJ,R*IFEQ                      . 377
         8,  ,
C
C
 PNKOPTAB:,Z00,KOPTAB
 ,Z00,PNKOPTAB
C
C
C***************************************
C*
C*   ПOCTOЯHHOE PACШИPEHИE ACCOЦИATИB-
C*   HOГO ПEPEKЛЮЧATEЛЯ ИHCTPYKЦИЙ.
C*
C
 EXTEND:  ,TEXT,8H     Z37
          ,UJ,R*ZL                        . 002 00  00
         7,Z30,                           . 370B
          ,TEXT,8H     INT
          ,UJ,R*INT                       . 005 02  01
         8,  ,
          ,TEXT,8H   OPSYN
          ,UJ,R*OPSYN                     . 006 04  02
         8,  ,
          ,TEXT,8H    IFLE
          ,UJ,R*IFLE                      . 010 06  03
         8,  ,
          ,TEXT,8H     E-X
          ,UJ,RE8KOP                      . 014 08  04
          ,Z25,                           . 025B
          ,TEXT,8H     A*X
          ,UJ,RE8KOP                      . 034 10  05
          ,Z17,                           . 017B
          ,TEXT,8H    EDIT
          ,UJ,R*EDIT                      . 036 12  06
         8,  ,
 :,TEXT,8H    *UTC
 ,UJ,*VMET6 . 040 14  07
         8,  ,
          ,TEXT,8H    VTMS
          ,UJ,RELONG/                     . 042 16  08
         5,Z00,                           . 240B
          ,TEXT,8H   STOPD
          ,UJ,R*ENDIF                     . 043 18  09
         8,  ,
          ,TEXT,8H     J+M
          ,UJ,RE8KOP                      . 056 20  10
         1,Z05,                           . 045B
          ,TEXT,8H   IFSEQ
          ,UJ,R*IFSEQ                     . 060 22  11
         8,  ,
          ,TEXT,8H     MAX
          ,UJ,R*MAX                       . 104 24  12
         8,  ,
          ,TEXT,8H     SST
          ,UJ,R*SST                       . 112 26  13
         8,Z00,EXTEND+96
          ,TEXT,8H      SJ
          ,UJ,*RELON2                     . 137 28  14
         1,Z34,                           . 074B
          ,TEXT,8H     Z14
          ,UJ,R*Z                         . 154 30  15
         3,Z00,                           . 140B
          ,TEXT,8H     SSW
          ,UJ,R*SSW                       . 155 32 16
         8,  ,
          ,TEXT,8H    HERE
          ,UJ,R*HERE                      . 162 34  17
         8,  ,
          ,TEXT,8H   EQUAL
          ,UJ,R*EQU                       . 166 36  18
         8,  ,
          ,TEXT,8H    IFNE
          ,UJ,R*IFNE                      . 173 38  19
         8,  ,
 ENDM*T:  ,TEXT,8H    ENDM
          ,UJ,R*ENDIF                     . 174 40  20
 10,,
          ,TEXT,8H     *73
          ,UJ,*RELON2                     . 201 42  21
         1,Z33,*+1                        . 073B
 I*T:     ,TEXT,8H       I
          ,UJ,*VMET6                      . 201 44  22
 10,,
          ,TEXT,8H     TEL
          ,UJ,R*TEL                       . 206 46  23
         8,  ,
 D*T:     ,TEXT,8H       D
          ,UJ,R*D                         . 235 48  24
 10,,
          ,TEXT,8H    DATA
          ,UJ,R*DATA                      . 243 50  25
         8,  ,
          ,TEXT,8H   EJECT
          ,UJ,R*EJECT                     . 251 52  26
         8,  ,
          ,TEXT,8H   IFSNE
          ,UJ,R*IFSNE                     . 254 54  27
         8,  ,
          ,TEXT,8H     REL
          ,UJ,R*REL                       . 262 56  28
         8,  ,
          ,TEXT,8H   PUNCH
          ,UJ,R*PUNCH                     . 265 58  29
         8,  ,
          ,TEXT,8H   8TEXT
          ,UJ,R*8TEXT                     . 273 60  30
         8,  ,
          ,TEXT,8H     *57
          ,UJ,*RELON2                     . 221 62  31
         1,Z17,                           . 057B
 LOCAL*T: ,TEXT,8H   LOCAL
          ,UJ,*VMET6                      . 225 64  32
 10,,
          ,TEXT,8H   LABEL
          ,UJ,R*LABEL                     . 305 66  33
         8,  ,
          ,TEXT,8H   MICRO
          ,UJ,R*MICRO                     . 313 68  34
         8,  ,
          ,TEXT,8H    LIST
          ,UJ,R*LIST                      . 027 70  35
         8,  ,
          ,TEXT,8H      RJ
          ,UJ,R*RJ                        . 045 72  36
         8,  ,
 REG*T:   ,TEXT,8H     REG
          ,UJ,*VMET6                      . 315 74  37
 10,,
          ,TEXT,8H     *55
          ,UJ,*RELON2                     . 317 76  38
         1,Z15,                           . 055B
          ,TEXT,8H    BASE
          ,UJ,R*BASE                      . 342 78  39
         8,  ,
          ,TEXT,8H     Z22
          ,UJ,R*ZL                        . 344 80  40
         4,Z20,*+1                        . 220B
          ,TEXT,8H    DMIC
          ,UJ,R*DMIC                      . 344 82  41
         8,  ,
          ,TEXT,8H    IFGE
          ,UJ,R*IFGE                      . 347 84  42
         8,  ,
          ,TEXT,8H     AMX
          ,UJ,RE8KOP                      . 376 86  43
          ,Z07,                           . 007B
          ,TEXT,8H     Z33
          ,UJ,R*ZL                        . 176 88  44
         6,Z30,                           . 330B
          ,TEXT,8H     E-C
          ,UJ,R*EMC                       . 031 90  45
         8,    ,
          ,TEXT,8H     E+C
          ,UJ,R*EPC                       . 123 92  46
         8,    ,
          ,TEXT,8H     ASC
          ,UJ,R*ASC                       . 145 94  47
 8,,
          ,TEXT,8H     M+J
          ,UJ,RE8KOP                      . 112 96  48
         1,Z05,                           . 045B
          ,TEXT,8H     ИПЗ
          ,UJ,R*JIF                       . 116 98  49
         8,  ,
 ,TEXT,8H     EXP
 ,UJ,R*EXP . 032 100 50
 8,,
 ,TEXT,8H      LN
 ,UJ,R*LN . 117 102 51
 8,,
 ,TEXT,8H   STIME
 ,UJ,R*STIME . 133 104 52
 8,,
 ,TEXT,8H    SQRT
 ,UJ,R*SQRT . 325 106 53
 8,,
 ,TEXT,8H    ASIN
 ,UJ,R*ASIN . 372 108 54
 8,,
 1,SET,R*END
 1,,STROKA+IFPLG
 ,END,MADLEN ASSEMBLER
 