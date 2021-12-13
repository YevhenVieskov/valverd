C****      
  100 FORMAT(//10X,'пюявер омеблнопхбндю *RP*',
     &       /10X,'================== ===')
  102 FORMAT(/5X, '*?* дсакхпнбюрэ бшбнд мю оевюрэ' 
     &       '<Y/N>' )
  104 FORMAT(/5X, '*?* ондцнрнбэре оевюрючыее сярпниярбн' 
     &       '<бй>' )
  106 FORMAT(/10X, '*?* хяундмше дюммше:' 
     &       10X, '--------------------' ) 
  108 FORMAT(/10X, 'онйюгюрекэ юдхюаюрш               :',  E13.5) 
  110 FORMAT(/10X, 'цюгнбюъ онярнъммюъ (дф/йц*й)      :',  E13.5) 
  112 FORMAT(/10X, 'релоепюрспю б люцхярпюкх (цпюд я) :',  E13.5) 
  114 FORMAT(/10X, 'дюбкемхе б люцхярпюкх (лою):',  E13.5) 
  116 FORMAT(/10X, 'дюбкемхе мю бшунде (лою):',  E13.5) 
  118 FORMAT(/10X, 'щтт. окныюдэ мю бунде:',  E13.5) 
  120 FORMAT(/10X, 'щтт. окныюдэ мю бшунде:',  E13.5) 
  122 FORMAT(/10X, 'люяяю опхбндю (йц):',  E13.5) 
  124 FORMAT(/10X, 'дхюлерп онпьмъ (л):',  E13.5) 
  125 FORMAT(/10X, 'дхюлерп оепбнцн ьрнйю (л):',  E13.5) 
  126 FORMAT(/10X, 'дхюлерп брнпнцн ьрнйю (л):',  E13.5) 
  128 FORMAT(/10X, 'люйяхлюкэмши унд (л):',  E13.5) 
  130 FORMAT(/10X, 'оюпюлерп X01:',  E13.5) 
  132 FORMAT(/10X, 'оюпюлерп X01:',  E13.5) 
  134 FORMAT(/10X, 'яхкю рпемхъ онйнъ:',  E13.5) 
  136 FORMAT(/10X, 'F1=',  E13.5, 3X,  'BTK=',  F7.4,
     &        3X,  'K1=',  F7.4,  3X,  'K3=',  F7.4,
     &       /10X, 'F2=',  E13.5, 3X,  'FIK=',  F7.4,
     &        3X,  'K2=',  F7.4,  3X,  'KK=',  F7.4) 
  140 FORMAT(//10X, 'мювюкэмше дюммше:',  
     &       /10X,  '-----------------') 
  142 FORMAT(/10X, 'унд онпьмъ (л)               :',  E13.5) 
  144 FORMAT(/10X, 'яйнпнярэ онпьмъ (л)          :',  E13.5) 
  146 FORMAT(/10X, 'дюбкемхе б 1-и онкнярх (лою) :',  E13.5) 
  148 FORMAT(/10X, 'дюбкемхе бн 2-и онкнярх (лою):',  E13.5) 
  150 FORMAT(/10X, 'ьюц хмрецпхпнбюмхъ (я)       :',  E13.5) 
  152 FORMAT(/10X, 'вепег яйнкэйн ьюцнб хмрецпхпнбюмхъ',
     &             'бшонкмърэ ббнд' /10X, 'мю дхяокеи :')
  154 FORMAT(/10X, 'мю оевюрэ :')
  160 FORMAT(/,' T (я)', 'X (л)', 'V (л/я)', 'P1(лою)',
     &      'P2 (лою)', 'PP (йм)', 'G1 (йця/я)', 'G2 (йця/я)')
  162 FORMAT(F9.3,5F8.3,2F10.5)
  166 FORMAT(/,  '*?* <бй>-опнднкфхрэ, N+<бй>-мнбше мювюкэмше дюммше,
     &       E+<бй>- гюйнмвхрэ=>')
  180 FORMAT(80X)
C     ****
  200 FORMAT (A1)
C================
      DIMENSION Y(4),YY(4),DY(4)
      REAL MS,KA,K1,K2,K3,KK
      INTEGER REPLY
      LOGICAL VVOD,DUBL
      COMMON /VD/ VVOD,DOUBLE
      VVOD=.TRUE.
      DUBL=.FALSE.
C================
      
      TYPE 100
      TYPE 102
      ACCEPT 200, REPLY
      IF(REPLY.EQ.'Y') DUBL=.TRUE.
      IF(.NOT.DUBL) GOTO 5
      TYPE 104
      ACCEPT 200, REPLY
      PRINT 100
      PRINT 106
    5 TYPE 106
C*************ббнд оюпюлерпнб***
      CALL RPSUB(T,X,V,PP)
      TYPE 108
      ACCEPT *,KA
      TYPE 110
      ACCEPT *,R
      TYPE 112
      ACCEPT *,TMP
      TYPE 114
      ACCEPT *,PM
      TYPE 116
      ACCEPT *,PA
      TYPE 118
      ACCEPT *,FE1
      TYPE 120
      ACCEPT *,FE2
      TYPE 122
      ACCEPT *,MS
      TYPE 124
      ACCEPT *,D1
      TYPE 125
      ACCEPT *,D2
      TYPE 126
      ACCEPT *,D3
      TYPE 128
      ACCEPT *,XMAX
      TYPE 130
      ACCEPT *,X01
      TYPE 132
      ACCEPT *,X02
      TYPE 134
      ACCEPT *,TRP
C     ***
      VVOD=.FALSE.
      FP1=3.1415*0.25*(D1**2-D3**2)
      FP2=3.1415*0.25*(D1**2-D2**2)
      K1=2./KA
      K2=(KA+1.)/KA
      K3=(KA-1.)/KA
      KK=SQRT(2.*KA/(KA-1))
      BTK=(2./(KA+1))**(KA/(KA-1.))
      FIK=SQRT(BTK**K1-BTK**K2)
      TYPE 136, FP1,BTK,K1,K3,FP2,FIK,K2,KK
      IF(.NOT.DUBL) GO TO 7
C*************оевюрэ оюпюлерпнб***
      PRINT 108, KA
      PRINT 110, R
      PRINT 112, TMP
      PRINT 114, PM
      PRINT 116, PA
      PRINT 118, FE1
      PRINT 120, FE2
      PRINT 122, MS
      PRINT 124, D1
      PRINT 125, D2
      PRINT 126, D3
      PRINT 128, XMAX
      PRINT 130, X01
      PRINT 132, X02
      PRINT 134, TRP
      PRINT 136, FP1, BTK, K1, K3, FP2, FIK, K2, KK
    7 PM=(PM+0.1)*1E-6
      PA=(PA+0.1)*1E-6
      TMP=TMP+273.
C     ***
   10 IF(.NOT.DUBL) GO TO 20
      DO 15 I=1,6
   15 PRINT 180
      TYPE 104
      ACCEPT 200, REPLY
   20 CONTINUE
C*************мювюкэмше дюммше*** 
      TYPE 140
      TYPE 142
      ACCEPT *, X
      TYPE 144
      ACCEPT *, V
      TYPE 146
      ACCEPT *, P1
      TYPE 148
      ACCEPT *, P2
      TYPE 150
      ACCEPT *, DT
      TYPE 152
      ACCEPT *, NSD
      IF(NSD.LE.0) NSD=1
      IF(.NOT.DUBL) GO TO 25
      TYPE 154
      ACCEPT *, NSP
      IF(NSP.LE.0) NSP=1
      PRINT 140
      PRINT 142, X
      PRINT 144, V
      PRINT 146, P1
      PRINT 148, P2
      PRINT 150, DT
      DO 22 I=1,6
   22 PRINT 180
      TYPE 104
      ACCEPT 200,REPLY
      PRINT 160
      PRINT 180
C*************дюбкемхе б ях*** 
   25 P1=(P1+0.1)*1E-6
      P2=(P2+0.1)*1E-6
      ISD=0
      ISP=0
      ISS=0
      T=0
      TYPE 160
      TYPE 180
      GO TO 40
C*************бшбнд пегскэрюрнб***
   30 TYPE 160
      TYPE 180
   33 IF(ISD.NE.0) GO TO 35
      TYPE 162,T,X,V,P1OUT,P2OUT,PPOUT,G1,G2
      ISS=ISS+1
   35 IF(.NOT.DUBL) GO TO 40
      IF(ISP.NE.0) GO TO 40
      PRINT 162,T,X,V,P1OUT,P2OUT,PPOUT,G1,G2
C*************мювюкн яверю***     
   40 CALL RPSUB(T,X,V,PP)
      PS=P1*FP1-P2*FP2-PP
      IF(V.EQ.0.) W=PS-SIGN(TRP,PS)
      IF(V.NE.0.) W=PS-SIGN(TRP,V)
      W=W/MS
C*************онпьемэ лнфер оепелеыюрэяъ*** 
      IF(ABS(PS).GT.TRP) GO TO 55
      DV=W*DT
      IF(DV*V.LT.0. .AND. ABS(DV).LT.ABS(V)) GO TO 55
C*************онпьемэ б онйне хг-гю рпемхъ*** 
      V=0.
      W=0.
C***      
   55 Y(1)=X
      DY(1)=V
      Y(2)=V
      DY(2)=W
      IF(P1.GT.PM) GO TO 65      
C*************1-ъ онкнярэ брейюмхе*** 
      BT=P1/PM
      FI=FIK
      IF(BT.GT.FIK) FI=SQRT(BT**K1-BT**K2)
      G1=FE1*KK*PM*FI/SQRT(R*TMP)
      DY(3)=KA*R*TMP*G1-KA*P1*FP1*V
      DY(3)=DY(3)/(FP1*(X01+X))
      Y(3)=P1
      GO TO 70
C*************1-ъ онкнярэ бшрейюмхе*** 
  65  BT=P1/PM
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G1=-FE1*KK*P1*FI/SQRT(R*TMP1)
      DY(3)=-KA*R*TMP1*(-G1)-KA*P2*FP2*V
      DY(3)=DY(3)/(FP1*(X01+X))
      Y(3)=P1
C*** 
   70 IF(P2.GT.PA) GO TO 75
C*************2-ъ онкнярэ брейюмхе*** 
      BT=P2/PA
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G2=FE2*KK*PA*FI/SQRT(R*TMP)
      DY(4)=KA*R*TMP*G2+KA*P2*FP2*V
      DY(4)=DY(4)/(FP2*(X02+XMAX-X))
      Y(4)=P2
      GO TO 80
C*************2-ъ онкнярэ бшрейюмхе*** 
   75 BT=PA/P2
      TMP2=TMP*(P2/PM)**K3
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G2=-FE2*KK*P2*FI/SQRT(R*TMP2)
      DY(4)=-KA*R*TMP2*(-G2)+KA*P2*FP2*V
      DY(4)=DY(4)/(FP2*(X02+XMAX-X))
      Y(4)=P2
      GO TO 80
C*************ьюц хмрецпхпнбюмхъ лернднл щикепю*** 
   80 DO 81 I=1,4
   81 Y(I)=Y(I)+DY(I)*DT
      T=T+DT
      X=Y(1)
      V=Y(2)
      P1=Y(3)
      P2=Y(4)
C***
      P1OUT=P1*1.E-6-0.1
      P2OUT=P2*1.E-6-0.1
      PSOUT=PS*1.E-3
      IF(X.GT.0.) GO TO 85      
C*************нцпюмхвемхе ундю якебю*** 
      X=0.
      IF(V.LT.0.) V=0.
      GO TO 90
   85 IF(X.LT.XMAX) GO TO 90    
C*************нцпюмхвемхе ундю яопюбю*** 
      X=XMAX
      IF(V.GT.0.) V=0.
C***
   90 ISD=ISD+1
      ISP=ISP+1
      IF(ISD.GE.NSD) ISD=0
      IF(ISP.GE.NSP) ISP=0
      IF(ISS.LT.20) GO TO 33
      ISS=0
      TYPE 166
      ACCEPT 200,REPLY
      IF(REPLY.EQ.'E') STOP     !RETURN
      IF(REPLY.NE. 'N') GO TO 30
      GO TO 10
      END
      
      SUBROUTINE RPSUB(T,X,V,PP)
      LOGICAL VVOD,DUBL
      COMMON /VD/ VVOD,DOUBLE
  100 FORMAT(/10X, 'онкегмюъ мюцпсгйю :',  
     &       /10X, 'онярнъммюъ янярюбкъчыюъ (м) :', E13.5)
  105 FORMAT(/10X, 'йнщттхжхемр опх X      (м/л):',  E13.5)
  110 FORMAT(/10X, 'йнщттхжхемр опх V      (м*я/л):',  E13.5)
      IF(.NOT.VVOD) GO TO 10
C*************ббнд***
      TYPE 100
      ACCEPT*,PP0
      TYPE 105
      ACCEPT*,PPX
      TYPE 110
      ACCEPT*,PPV
      IF(.NOT.DUBL) RETURN
      PRINT 100,PP0
      PRINT 105,PPX
      PRINT 110,PPV
C*************пюявер***
   10 PP=PP0+PPX*X+PPV*V
      RETURN
      END
      
      
      