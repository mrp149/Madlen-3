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
MDL*REF:,NAME,
 ,TITLE,ACCEMБЛEP MADLEN. ГEHEPATOP CCЫЛOK.
 LISTS:LC,BLOCK,SECTOR(5120)
 TABDEC:,EQU,LISTS+4096
 BUFPER:,SUBP,
 STROKA:,EQU,BUFPER+4012B
 ENTRY*:,LC,
 SOS:,EQU,251B
 SPASAN*:,LC,
 CN00001:,EQU,135B
 CN00007:,EQU,143B
 CN000001:,EQU,145B
 CN000002:,EQU,146B
 CN000003:,EQU,147B
 NO*DEC:,EQU,250B
 PRINT8**:,SUBP,
 FOMA***:,SUBP,
 SPACE:,EQU,FOMA***+36B
 FLAGTABL:,EQU,41
 NK:,EQU,52
 NKCARD:,EQU,54
 MSK7*5:,EQU,60
 CON1:,EQU,76
 CON2:,EQU,77
 CON4:,EQU,79
 CON7:,EQU,81
 RCON1:,EQU,108
 CONR15:,EQU,109
 CNR7S12:,EQU,112
 STN1:,EQU,329
 TITLE2:,EQU,198
 MESMOD:,EQU,294
 MESADR3:,EQU,308
 RCON377:,EQU,398
 MASK33:,EQU,85
 RBLANK4:,EQU,138
 *PROBEL:,EQU,234
 LISMASK:,EQU,402
 LISLAST:,EQU,405
 LIS*R:,EQU,412
 CNR40000:,EQU,LIS*R
 EXT:,EQU,514
C***************** BXOДЫ B MADLENS*
C*   MADLENS*+1  ГEHEPAЦИЯ ИДEHT.
C*   MADLENS*    CCЫЛKA HA ИДEHT.
C*   14ИP BOЗBPAT ИЗ MADLENS*
C*
 C:,BSS,
 ,ITA,14
 ,UJ,C*C*
 :,XTA,
 C*C*:,UTC,PIPIPI
 ,ATX,
 ,ITA,14
 14,VTM,-12
 C*:14,ITS,13
 14,VLM,C*
 ,XTS,
 7,BASE,C
 12,VZM,PRINT
 12,UTM,-LISTS
 ,ITA,12
 ,ASN,64+1
 ,ATI,12
 C1:6,XTA,FLAGTABL
 6,AAX,RCON1
 0,U1A,P2PR
 ,XTA,PI1
 ,U1A,P*1
 ,WTC,17B
 1,VTM,8
C    POCПИCЬ 0 ДBYX ЛИCTOB ПAMЯTИ
 14,VTM,1-2048
 P*0:,XTA,
 1,ATX,
 1,UTM,1
 14,VLM,P*0
 6,XTA,CON2
 ,ATX,PI1
 P*1:12,MTJ,14
 ,WTC,17B
 14,UTM,8
 14,XTA,
 ,U1A,P*2
 ,ITA,12
 ,ASN,64-1
 ,ATI,1
 1,UTM,LISTS
 ,ITA,1
 P*2:6,ARX,CN000001
 ,XTS,PIPIPI
 ,U1A,P*2A
 11,VJM,PPPZ
 15,AOX,
 ,XTS,
 P*2A:,STX,
 14,ATX,
 ,UJ,OUT
 PPPZ:6,XTA,NK
 6,AOX,STN1
 :,UZA,PPPL
 6,XTA,NK
 :6,ARX,SOS
 6,ARX,RCON1
 :,ASN,64-30
 11,UJ,
 :6,XTA,NK
 ,UJ,PPPZ+2
 PPPL:6,XTA,NKCARD
 6,ARX,RCON1
 15,ATX,1
 ,ASN,64+12
 ,U1A,PPPZ+4
 15,XTA,1
 6,AEX,CNR7S12
 ,UJ,PPPZ+3
 P2PR:,BSS,
 ,XTA,PI2
 ,U1A,P/7
 6,XTA,CON2
 ,ATX,PI2
 P2NOIS:,BSS,
C    ПPOДBИЖKA PAБOЧEГO ПOЛЯ ПO EXT
 6,WTC,EXT
 1,VTM,
 ,WTC,17B
 2,VTM,8
 P/0:2,XTA,
 ,U1A,P/1
 P//0:2,UTM,1
 ,ITA,2
C    ПPOBEPKA HA KOHEЦ TAБЛИЦЫ
 ,WTC,17B
 14,VTM,8+2048
 ,ITS,14
 15,AEX,
 ,U1A,P/0
 ,UJ,P/7
 P/1:6,AOX,CON2
 2,ATX,
 ,AAX,=77777 00000
 ,ATX,PN
 2,XTA,
 ,ATI,14
 14,XTA,1
 6,AAX,CN00007
 6,AEX,CN00001
 ,U1A,P/3DY
 14,XTA,1
 ,AAX,=700000
 ,U1A,P/3DY
 14,XTA,1
 ,AAX,=77777
 6,ARX,SOS
 ,ARX,=1
 ,ASN,64-30
 2,XTS,
 ,AAX,=7 00000 77777 77777
 15,AOX,
 2,ATX,
 P/3DY:,XTA,PN
 6,AEX,CN000001
 ,UZA,P/2
 6,AEX,CN000003
 ,U1A,P/3
 P/2:2,XTA,
 ,AAX,=7 77777 00000 77777
 2,ATX,
 ,UJ,P//0
 P/3:2,XTA,
 ,AEX,PN
 6,AOX,CON1
 ,ITS,1
 ,ASN,64-15
 15,AOX,
 6,AOX,CON4
 2,ATX,
 ,XTA,PN
 ,ASN,64+15
 ,ATI,14
 14,UTM,-1
 ,XTA,
 1,ATX,
 P4:1,UTM,1
 ,ITA,1
 ,AEX,16B
 6,AAX,CONR15
 ,UZA,PDIAG
 ,XTA,
 1,ATX,
 14,UTM,-3
 14,VZM,P/5
 ,ITA,14
 6,AAX,CNR40000
 ,UZA,P4
 P/5:,UJ,P//0
 PDIAG:6,XTA,CON2
 ,ATX,DIAG
 ,XTA,PD
 ,CTX,C1
 PD:,UJ,OUT
 P/7:,XTA,PI2
 ,UZA,PRI
 ,WTC,17B
 12,UTM,8
 6,XTA,NK
 6,AOX,STN1
 11,VJM,PPPZ+1
 :,ATX,PS
 12,XTA,
 ,NTR,13B
 ,UZA,P/8
 ,XTS,PS
 ,ASN,64+15
 15,AEX,
 12,ATX,
 ,UJ,OUT
 P/8:,ASN,64+15
 ,ATI,3
 P/8A:3,XTA,
 ,NTR,13B
 ,UZA,P/9
 6,AAX,CONR15
 ,UZA,P/10
 3,XTA,
 6,AOX,CON4
 3,ATX,
 P/9:3,UTM,1
 ,UJ,P/8A
 P/10:,XTA,=:0 77777
 ,ATX,PR
 P/9A:3,XTA,
 ,AAX,PR
 ,UZA,PR10
 ,XTA,PR
 ,ASN,64+15
 ,ATX,PR
 ,XTA,PS
 ,ASN,64+15
 ,ATX,PS
 ,UJ,P/9A
 PR10:3,XTA,
 ,AOX,PS
 3,ATX,
 OUT:14,VTM,13
 ,STI,
 :14,STI,
 14,UTM,-1
 14,V1M,*-1
 ,ATI,14
 14,UJ,
 PRINT:,BSS,
 6,XTA,LISMASK
 6,AOX,LISLAST
 15,ATX,
 ,ASN,64+6
 15,AAX,
 6,AAX,LIS*R
 ,UZA,OUT
 5,VTM,ENTRY*
 5,XTA,2
 ,UZA,PRINT1
 11,VTM,TEX1
 13,VJM,PRIMA
 SP:5,XTA,
 12,VJM,PERID
 :6,AAX,MASK33
 4,VTM,SPASAN*+2
 4,STX,-1
 4,ATX,-2
 5,XTA,1
 6,ARX,RCON1
 6,ARX,SOS
 ,ASN,64-33
 12,VJM,MADESS
 5,UTM,2
 12,VJM,PRIN
 5,XTA,
 ,U1A,SP
 PRINT1:,BSS,
 ,XTA,PI1
 ,UZA,OUT
 ,XTA,PI2
 ,UZA,P2NOIS
 ,XTA,DIAG
 ,UZA,PRI
 11,VTM,TEX4
 13,VTM,OUT
 ,ITA,13
 15,ATX,
 ,UJ,PRIMA1
 PRI:,BSS,
 1,VTM,
 2,VTM,-2047
 PR/:,WTC,17B
 2,XTA,8+2047
 ,UZA,PR1
 ,WTC,17B
 1,ATX,8
 1,UTM,1
 PR1:2,VLM,PR/
 1,MTJ,2
 2,VZM,OUT
 ,WTC,17B
 3,VTM,8
 1,MTJ,9
 13,VJM,YPOR
 ,XTA,PI2
 ,UZA,NOISP
 TT:3,XTA,
 ,AAX,=77777 00000
 ,UZA,TTT
 ,XTA,DYKA
 ,U1A,TT1
 11,VTM,TEX2
 13,VJM,PRIMA
 6,XTA,RCON1
 ,ATX,DYKA
 TT1:3,XTA,
 13,VJM,PRINTP
 TTT:,BSS,
 2,UTM,-1
 2,VZM,NOISP
 3,VLM,TT
 PRINTP:,BSS,
 ,ATX,PN
 ,WTC,PN
 ,XTA,
 12,VJM,PERID
 :6,AEX,RBLANK4
 4,VTM,SPASAN*
 4,STX,1
 4,ATX,
 12,VJM,TOP
 4,UTM,3
 ,XTA,PN
 ,AAX,=:0 77777
 ,ASN,64-3
 12,VJM,MADESS
 ,XTA,PN
 ,NTR,13B
 ,UZA,YESPR
 ,XTA,PN
 ,AAX,=:0 00000 77777
 ,UZA,SS1
 ,ASN,64-18
 12,VJM,MADESS
 SS1:,BSS,
 13,MTJ,12
 ,UJ,PRIN
 YESPR:,BSS,
 5,VTM,1
 ,XTA,PN
 ,ASN,64+15
 ,ATI,8
 ,ITA,8
 ,ASN,64-2
 ,ATX,PS
 FI*:5,MTJ,12
 12,UTM,-7
 12,VZM,FIFIN
 FI:,XTA,PS
 6,ARX,RCON1
 ,ATX,PS
 ,AAX,=3
 ,U1A,FI1
 ,XTA,PS
 ,ASN,64+2
 ,ATI,14
 14,UTM,-1
 14,XTA,
 ,NTR,13B
 ,U1A,FIFIFI
 ,UJ,FI
 FI1:5,UTM,1
 12,VJM,READ
 ,UZA,FIFIFI
 12,VJM,MADESS
 ,UJ,FI*
 FIFIFI:8,VTM,
 5,VZM,YBFIN
 FIFIN:,BSS,
 12,VJM,PRIN
 6,XTA,*PROBEL
 4,VTM,SPASAN*
 4,ATX,
 4,ATX,2
 6,XTA,CON1
 4,ATX,1
 4,UTM,3
 5,VTM,
 8,VZM,YBFIN
 ,UJ,FI*
 MADESS:15,ATX,1
 6,AAX,CON7
 6,AEX,CON7
 ,U1A,MADESSC
 15,XTA,1
 ,ARX,=:77777
 6,AUX,MSK7*5
 ,ASN,64-8
 ,AEX,=6H0000L
 ,UJ,MADESS1
 MADESSC:15,XTA,1
 ,ARX,=:77777
 6,AUX,MSK7*5
 ,AOX,=6H00000
 MADESS1:4,ATX,
 6,XTA,CON1
 4,ATX,1
 4,UTM,2
 12,UJ,
 READ:,XTA,PS
 ,ASN,64+2
 ,ATI,10
 ,ASN,64-2
 ,AEX,PS
 ,ATI,14
 10,XTA,
 14,UTC,RFIN-1
 ,ASX,
 12,UJ,
 RFIN:,BSS,
 ,OCT,364
 ,OCT,270
 ,OCT,174
 PRIN:,BSS,
 14,VTM,SPASAN*
 14,XTA,2
 ,AEX,=6H *** ''
 ,U1A,PRIN1
 6,XTA,*PROBEL
 14,ATX,3
 PRIN1:,BSS,
 ,ITA,13
 ,ITS,14
 4,UTM,-1
 ,ITS,4
 6,XTS,RCON1
 13,VJM,PRINT8**
 ,ATI,13
 12,UJ,
 YBFIN:,BSS,
 13,UJ,
 TOP:,WTC,PN
 ,XTA,1
 15,ATX,1
 6,AEX,NO*DEC
 14,VTM,37B
 ,UZA,SETTOP
 15,XTA,1
 ,AAX,=:023
 ,UZA,TESGLOB
 ,APX,=:023
 ,ASN,64-3
 ,YTA,
 :,ATI,14
 14,UTM,37B
 SETTOP:,UTC,T
 14,XTA,
 ,AAX,=:00177777776
 ,AEX,=:100000000002
 4,ATX,2
 12,UJ,
 TESGLOB:15,XTA,1
 ,AAX,=17400000
 ,UZA,TESGLOB1
 ,ASN,64+1
 15,XTS,
 6,AAX,CN000002
 15,ARX,
 ,ASN,64+16
 ,ATI,14
 ,UJ,SETTOP
 TESGLOB1:15,XTA,1
 6,AAX,CN000003
 14,VTM,
 ,UZA,SETTOP
 6,AEX,CN000001
 14,VTM,5
 ,U1A,SETTOP
 15,XTA,1
 ,ASN,64+24
 ,ATI,14
 14,UTC,TABDEC-4000B
 ,XTA,
 15,ATX,2
 6,AAX,CN00007
 ,UZA,TESGLOB2
 15,XTA,2
 ,AAX,=17400000
 ,ASN,64+17
 6,ARX,RCON1
 ,UJ,SETTOP-1
 TESGLOB2:15,XTA,2
 ,APX,=50200000
 ,ASN,64-3
 ,YTA,
 ,ATI,14
 ,ASN,64+1
 14,UTM,61B
 ,UZA,SETTOP
 15,XTA,1
 6,AAX,CONR15
 ,UZA,SETTOP
 14,UTM,2
 ,UJ,SETTOP
 MDL*6T8W:,ENTRY,
 PERID:15,ATX,4
 ,ITA,
 6,XTS,*PROBEL
 15,ATX,5
 ,ITS,12
 ,ITS,7
 15,XTS,
 7,VTM,C
 14,VTM,-32
 12,VTM,-5
 P*CY:,UZA,P*RET
 ,ASN,64-6
 15,ATX,
 ,YTA,
 15,ATX,
 15,WTC,
 7,XTA,T-C
 6,AEX,*PROBEL
 7,AAX,=177400-C
 14,ASN,64
 15,AEX,1
 15,STX,1
 14,UTM,8
 12,VLM,P*CY
 15,XTS,1
 15,ATX,-5
 6,XTA,*PROBEL
 15,STX,1
 14,VTM,-32
 12,VJM,P*CY
 P*RET:15,XTA,-4
 14,VTM,
 :,UZA,*+1
 14,VTM,1
 :15,XTA,2
 14,UTC,
 15,STX,-4
 ,STI,7
 ,STI,12
 12,UJ,
 MDL*6T8S:,ENTRY,
 :15,ATX,
 15,WTC,
 ,UTC,T
 ,XTA,
 ,UTC,=177400
 ,AAX,
 ,ASN,64+8
 12,UJ,
 YPOR:,BSS,
 ,ITA,13
 ,ATX,I13
 YPP:9,MTJ,12
 12,UTM,-1
 12,VZM,YOUT
 ,WTC,17B
 12,UTM,8
 ,ITA,12
 ,ATX,N9
 ,WTC,17B
 11,VTM,8
 ,XTA,
 ,ATX,XX
 NN:11,WTC,
 ,XTA,
 ,ATX,RA
 11,WTC,1
 ,XTA,
 ,ATX,RB
 CONT1:13,VJM,ALEB
 ,UZA,YFIN
 11,XTA,
 11,XTS,1
 11,STX,
 11,ATX,1
 ,ATX,XX
 YFIN:11,UTM,1
 ,ITA,11
 ,AEX,N9
 ,U1A,NN
 9,UTM,-1
 ,XTA,XX
 ,U1A,YPP
 YOUT:,WTC,I13
 ,UJ,
 ALEB:,BSS,
 ,ITA,5
 ,ITS,9
 ,ITS,11
 ,XTS,
 5,VTM,-7
 AL:,XTA,RA
 ,ASN,64-6
 ,ATX,RA
 ,YTA,
 ,ATI,12
 12,XTA,T
 6,AAX,RCON377
 ,ATX,RA1
 ,XTA,RB
 ,ASN,64-6
 ,ATX,RB
 ,YTA,
 ,ATI,12
 12,XTA,T
 6,AAX,RCON377
 ,ATX,RB1
 ,NTR,3
 ,A-X,RA1
 ,U1A,AL2
 ,XTA,RA1
 ,AEX,RB1
 ,U1A,AL3
 5,VLM,AL
 AL3:,STX,
 ,STI,11
 ,STI,9
 ,ATI,5
 ,XTA,
 13,UJ,
 AL2:,STX,
 ,STI,11
 ,STI,9
 ,ATI,5
 ,XTA,=1
 13,UJ,
 T:,ISO,6H =   '00' .00
 ,ISO,6H!=X !'01' .01
 ,ISO,6H(:  Б'55' .02
 ,ISO,6H)=: Ц'67' .03
 ,ISO,6H;=-:Д'57' .04
 ,ISO,6H+=-XФ'66' .05
 ,ISO,6H-SUBГ'56' .06
 ,ISO,6H*   И'62' .07
 ,ISO,6H/LS ('02' .10
 ,ISO,6H0PS )'03' .11
 ,ISO,6H1LU *'07' .12
 ,ISO,6H2PU Й'63' .13
 ,ISO,6H3LP Л'64' .14
 ,ISO,6H4PP Я'77' .15
 ,ISO,6H5LC Ж'60' .16
 ,ISO,6H6PC /'10' .17
 ,ISO,6H7   0'11' .20
 ,ISO,6H8   1'12' .21
 ,ISO,6H9D: 2'13' .22
 ,ISO,6HA=D 3'14' .23
 ,ISO,6HB=-D4'15' .24
 ,ISO,6HC   5'16' .25
 ,ISO,6HD   6'17' .26
 ,ISO,6HE   7'20' .27
 ,ISO,6HFSS 8'21' .30
 ,ISO,6HG   9'22' .31
 ,ISO,6HHSU Ь'74' .32
 ,ISO,6HI   ;'04' .33
 ,ISO,6HJSP П'65' .34
 ,ISO,6HK   +'05' .35
 ,ISO,6HLSC -'06' .36
 ,ISO,6HM***Ы'73' .37
 ,ISO,6HNSSTЗ'61' .40
 ,ISO,6HORDFA'23' .41
 ,ISO,6HP   B'24' .42
 ,ISO,6HQ*2*C'25' .43
 ,ISO,6HR*2*D'26' .44
 ,ISO,6HS*2*E'27' .45
 ,ISO,6HT=SBF'30' .46
 ,ISO,6HU   G'31' .47
 ,ISO,6HV=LSH'32' .50
 ,ISO,6HW=PSI'33' .51
 ,ISO,6HX=LUJ'34' .52
 ,ISO,6HY=PUK'35' .53
 ,ISO,6HZ=LPL'36' .54
 ,ISO,6HБ=PPM'37' .55
 ,ISO,6HГ=LCN'40' .56
 ,ISO,6HД=PCO'41' .57
 ,ISO,6HЖ   P'42' .60
 ,ISO,6HЗ=X Q'43' .61
 ,ISO,6HИ=X R'44' .62
 ,ISO,6HЙP*PS'45' .63
 ,ISO,6HЛP/PT'46' .64
 ,ISO,6HП=* U'47' .65
 ,ISO,6HФ=/ V'50' .66
 ,ISO,6HЦWEQW'51' .67
 ,ISO,6HЧ=SSX'52' .70
 ,ISO,6HШ=W Y'53' .71
 ,ISO,6HЩ=SUZ'54' .72
 ,ISO,6HЫ   Ш'71' .73
 ,ISO,6HЬ=SPЭ'75' .74
 ,ISO,6HЭ   Щ'72' .75
 ,ISO,6HЮ=SCЧ'70' .76
 ,ISO,6HЯ   Ю'76' .77
 NOISP:,BSS,
 1,MTJ,2
 ,WTC,17B
 3,VTM,8
 3,MTJ,10
 1,VTM,
 PI*:3,XTA,
 ,AAX,=77777 00000
 2,UTM,-1
 ,U1A,PI*A
 3,XTA,
 10,ATX,
 10,UTM,1
 1,UTM,1
 PI*A:,BSS,
 2,VZM,PI
 3,VLM,PI*
 PI:,BSS,
 1,VZM,OUT
 ,XTA,PI2
 ,U1A,*+1
 ,UTC,TEX5-TEX3
 11,VTM,TEX3
 13,VJM,PRIMA
 ,XTA,
 ,WTC,17B
 1,ATX,8
 1,MTJ,2
 5,VTM,
 /1/:2,UTM,-3
 5,UTM,1
 2,VZM,/2/
 ,ITA,2
 6,AAX,CNR40000
 ,UZA,/1/
 /2/:,BSS,
 8,VTM,.I
 ,WTC,17B
 3,VTM,8
 PI2U:8,MTJ,9.J
 4,VTM,SPASAN*
 10,VTM,
 ,UJ,PIA
 PI3:10,VTM,
 PI3A:10,UTM,1
 9,UTM,1
 3,UTC,
 9,XTA,
 ,UZA,PIPRI
 ,ITA,10
 ,ITS,5
 15,AEX,
 ,U1A,PI3A
 PIA:3,UTC,
 9,XTA,
 ,ATX,PN
 ,WTC,PN
 ,XTA,
 12,VJM,PERID
 :6,AEX,RBLANK4
 4,STX,1
 4,ATX,
 12,VJM,TOP
 4,UTM,3
 3,UTC,
 9,XTA,
 ,AAX,=:0 77777
 ,ASN,64-3
 12,VTM,PI3
 ,UJ,MADESS
 PIPRI:12,VJM,PRIN
 8,UTM,1
 ,ITA,8
 ,ITS,5
 15,AEX,
 ,U1A,PI2U
 ,UJ,OUT
 PRIMA:,ITA,13
 6,XTS,RCON1
 6,ATX,MESMOD
 ,XTA,=5
 6,ATX,MESADR3
 ,XTA,
 6,ATX,TITLE2
 13,VJM,SPACE
 PRIMA1:15,XTA,
 8,VTM,STROKA+*PROBEL
 8,MTJ,9
 13,VJM,PRIL
 11,MTJ,8
 11,MTJ,9
 9,UTM,2
 13,VJM,PRIL
 8,VTM,TEX
 9,VTM,TEX+2
 13,VJM,PRIL
 :11,XTS,
 6,AEX,CON1
 6,ATX,TITLE2
 :11,XTA,1
 6,ATX,TITLE2+1
 11,XTA,2
 6,ATX,TITLE2+2
 14,VTM,-6
 ,XTA,
 :6,UTC,TITLE2+9
 14,ATX,
 14,VLM,*-1
 6,STX,TITLE2+10
 8,VTM,STROKA+*PROBEL
 8,MTJ,9
 ,ATI,13
 PRIL:,ITS,8
 ,ITS,9
 ,XTS,=1
 ,UJ,PRINT8**
 TEX:,ISO,9H---------
 ,CONT,9H---------
 TEX1:,ISO,18H TAБЛИЦA BXOДOB :
 TEX2:,ISO,18H TAБЛИЦA CCЫЛOK :
 TEX3:,ISO,18H HE ИCПOЛЬЗOBAHЫ:
 TEX4:,ISO,18H HEXBATAET ПAMЯTИ
 TEX5:,ISO,18H ИДEHTИФИKATOPЫ :
 RV:,BSS,1
 RA:,BSS,1
 RB:,BSS,1
 RX:,BSS,1
 RB1:,BSS,1
 RA1:,BSS,1
 XX:,BSS,1
 I13:,BSS,1
 N9:,BSS,1
 PI1:,BSS,1
 PI2:,BSS,1
 PN:,BSS,1
 DIAG:,BSS,1
 PS:,BSS,1
 PR:,BSS,1
 PIPIPI:,BSS,1
 DYKA:,BSS,1
 ,END,
 