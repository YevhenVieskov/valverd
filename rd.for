       
      
      
C**** 
      PROGRAM RP
      use text_transfer
  100 FORMAT(//10X,'������ ������������� *RP*',
     &       /10X,'================== ===')
  102 FORMAT(/5X, '*?* ����������� ����� �� ������' 
     &       '<Y/N>' )
  104 FORMAT(/5X, '*?* ����������� ���������� ����������' 
     &       '<��>' )
  106 FORMAT(/10X, '*?* �������� ������:' 
     &       10X, '--------------------' ) 
  108 FORMAT(/10X, '���������� ��������               :',  E13.5) 
  110 FORMAT(/10X, '������� ���������� (��/��*�)      :',  E13.5) 
  112 FORMAT(/10X, '����������� � ���������� (���� �) :',  E13.5) 
  114 FORMAT(/10X, '�������� � ���������� (���):',  E13.5) 
  116 FORMAT(/10X, '�������� �� ������ (���):',  E13.5) 
  118 FORMAT(/10X, '���. ������� �� �����:',  E13.5) 
  120 FORMAT(/10X, '���. ������� �� ������:',  E13.5) 
  122 FORMAT(/10X, '����� ������� (��):',  E13.5) 
  124 FORMAT(/10X, '������� ������ (�):',  E13.5) 
  125 FORMAT(/10X, '������� ������� ����� (�):',  E13.5) 
  126 FORMAT(/10X, '������� ������� ����� (�):',  E13.5) 
  128 FORMAT(/10X, '������������ ��� (�):',  E13.5) 
  130 FORMAT(/10X, '�������� X01:',  E13.5) 
  132 FORMAT(/10X, '�������� X02:',  E13.5) 
  134 FORMAT(/10X, '���� ������ �����:',  E13.5) 
  136 FORMAT(/10X, 'F1=',  E13.5, 3X,  'BTK=',  F7.4,
     &        3X,  'K1=',  F7.4,  3X,  'K3=',  F7.4,
     &       /10X, 'F2=',  E13.5, 3X,  'FIK=',  F7.4,
     &        3X,  'K2=',  F7.4,  3X,  'KK=',  F7.4) 
  140 FORMAT(//10X, '��������� ������:',  
     &       /10X,  '-----------------') 
  142 FORMAT(/10X, '��� ������ (�)               :',  E13.5) 
  144 FORMAT(/10X, '�������� ������ (�)          :',  E13.5) 
  146 FORMAT(/10X, '�������� � 1-� ������� (���) :',  E13.5) 
  148 FORMAT(/10X, '�������� �� 2-� ������� (���):',  E13.5) 
  150 FORMAT(/10X, '��� �������������� (�)       :',  E13.5) 
  152 FORMAT(/10X, '����� ������� ����� ��������������',
     &             '��������� ����' /10X, '�� ������� :')
  154 FORMAT(/10X, '�� ������ :')
  160 FORMAT(/,'T (�)', 'X (�)', 'V (�/�)', 'P1(���)',
     &     'P2 (���)', 'PP (��)', 'G1 (���/�)', 'G2 (���/�)')
  162 FORMAT(F9.3,5F8.3,2F10.5)
  166 FORMAT(/,  '*?* <��>-����������, N+<��>-����� ��������� ������,
     &       E+<��>- ���������=>')
  180 FORMAT(80X)
C     ****
  200 FORMAT (A1)
C================
      DIMENSION Y(4),YY(4),DY(4)
      REAL MS,KA,K1,K2,K3,KK
      INTEGER REPLY
      LOGICAL VVOD,DUBL
C*******************������� ������******************
      CHARACTER(100) RUS100,RUS102,RUS104,RUS106,RUS108,RUS110,RUS112
      CHARACTER(100) RUS114,RUS116,RUS118,RUS120,RUS122,RUS124,RUS125
      CHARACTER(100) RUS126,RUS128,RUS130,RUS132,RUS134,RUS136,RUS140
      CHARACTER(100) RUS142,RUS144,RUS146,RUS148,RUS150,RUS152,RUS154
      CHARACTER(100) RUS160,RUS162,RUS166
      
C*******************������������ �������******************      
      CHARACTER(200) FMT100,FMT102,FMT104,FMT106,FMT108,FMT110,FMT112
      CHARACTER(200) FMT114,FMT116,FMT118,FMT120,FMT122,FMT124,FMT125
      CHARACTER(200) FMT126,FMT128,FMT130,FMT132,FMT134,FMT136,FMT140
      CHARACTER(200) FMT142,FMT144,FMT146,FMT148,FMT150,FMT152,FMT154
      CHARACTER(200) FMT160,FMT162,FMT166
      CHARACTER(100) RUSRP100,RUSRP105,RUSRP110
      CHARACTER(100) FMTRP100,FMTRP105,FMTRP110 
      INTEGER IOUT
      COMMON /VD/ VVOD,DUBL,IOUT
C*******************������������� ����������******************        
      X=0.
      V=0.
      PP0=0.
      PPV=0.
      PPX=0.
      PP=0.
      P1=0.
      P2=0.
      DT=0.
      IOUT=2
      OPEN(1,FILE="RESULT.TXT")
      OPEN(10,FILE="RESULT.csv")
      !OPEN(2,FILE="INPUT.TXT")
      RUS100="(//10X,'������ ������������� *RP*',
     &       /10X,'=====================')"  
      RUS102="(/5X, '*?* ����������� ����� �� ������',  '<Y/N>' )"
      RUS104="(/5X, '*?* ����������� ���������� ����������' 
     &       '<��>' )"
      RUS106="(/10X, '*?* �������� ������:' 
     &       10X, '--------------------' )"
      RUS108="(/10X, '���������� ��������               :',  E13.5)"
      RUS110="(/10X, '������� ���������� (��/��*�)      :',  E13.5)"
      RUS112="(/10X, '����������� � ���������� (���� �) :',  E13.5)"
      RUS114="(/10X, '�������� � ���������� (���):',  E13.5) "
      RUS116="(/10X, '�������� �� ������ (���):',  E13.5) "
      RUS118="(/10X, '���. ������� �� �����:',  E13.5) "
      RUS120="(/10X, '���. ������� �� ������:',  E13.5)"
      RUS122="(/10X, '����� ������� (��):',  E13.5)"
      RUS124="(/10X, '������� ������ (�):',  E13.5)"
      RUS125="(/10X, '������� ������� ����� (�):',  E13.5)"
      RUS126="(/10X, '������� ������� ����� (�):',  E13.5)"
      RUS128="(/10X, '������������ ��� (�):',  E13.5) "
      RUS130="(/10X, '�������� X01:',  E13.5)"
      RUS132="(/10X, '�������� X02:',  E13.5)"
      RUS134="(/10X, '���� ������ �����:',  E13.5)"
      RUS140="(//10X, '��������� ������:',  
     &       /10X,  '-----------------')" !!!!!!!!!
      RUS142="(/10X, '��� ������ (�)               :',  E13.5)"
      RUS144="(/10X, '�������� ������ (�)          :',  E13.5)"
      RUS146="(/10X, '�������� � 1-� ������� (���) :',  E13.5) "
      RUS148="(/10X, '�������� �� 2-� ������� (���):',  E13.5)"
      RUS150="(/10X, '��� �������������� (�)       :',  E13.5)"
      RUS152="(/10X, '����� ������� ����� �������������� ',
     &             '��������� ����' /10X, '�� ������� :')"          
      RUS154="(/10X, '�� ������ :')"
       RUS160="(/,'T (�)', 'X (�)', 'V (�/�)', 'P1(���)',
     &      'P2 (���)', 'PP (��)', 'G1 (���/�)', 'G2 (���/�)')"      
      RUS166="(/,  '*?* <��>-����������, N+<��>-����� ��������� ������,
     &       E+<��>- ���������=>')"

            
      RUSRP100="(/10X, '�������� �������� :',  
     &       /10X, '���������� ������������ (�) :', E13.5)"      
      RUSRP105="(/10X, '����������� ��� X      (�/�):',  E13.5)"
      RUSRP110="(/10X, '����������� ��� V      (�*�/�):',  E13.5)"
            
      
      FMT100=ru_doswin(RUS100,.FALSE.)
      FMT102=ru_doswin(RUS102,.FALSE.)
      FMT104=ru_doswin(RUS104,.FALSE.)
      FMT106=ru_doswin(RUS106,.FALSE.)
      FMT108=ru_doswin(RUS108,.FALSE.)
      FMT110=ru_doswin(RUS110,.FALSE.)
      FMT112=ru_doswin(RUS112,.FALSE.)
      FMT114=ru_doswin(RUS114,.FALSE.)
      FMT116=ru_doswin(RUS116,.FALSE.)
      FMT118=ru_doswin(RUS118,.FALSE.)
      FMT120=ru_doswin(RUS120,.FALSE.)
      FMT122=ru_doswin(RUS122,.FALSE.)
      FMT124=ru_doswin(RUS124,.FALSE.)
      FMT125=ru_doswin(RUS125,.FALSE.)
      FMT126=ru_doswin(RUS126,.FALSE.)
      FMT128=ru_doswin(RUS128,.FALSE.)
      FMT130=ru_doswin(RUS130,.FALSE.)
      FMT132=ru_doswin(RUS132,.FALSE.)
      FMT134=ru_doswin(RUS134,.FALSE.)
      FMT136=ru_doswin(RUS136,.FALSE.)
      FMT140=ru_doswin(RUS140,.FALSE.)
      FMT142=ru_doswin(RUS142,.FALSE.)
      FMT144=ru_doswin(RUS144,.FALSE.)
      FMT146=ru_doswin(RUS146,.FALSE.)
      FMT148=ru_doswin(RUS148,.FALSE.)
      FMT150=ru_doswin(RUS150,.FALSE.)
      FMT152=ru_doswin(RUS152,.FALSE.)
      FMT154=ru_doswin(RUS154,.FALSE.)
      FMT160=ru_doswin(RUS160,.FALSE.)
      FMT162=ru_doswin(RUS162,.FALSE.)
      FMT166=ru_doswin(RUS166,.FALSE.)
      
      FMTRP100=ru_doswin(RUSRP100,.FALSE.)
      FMTRP105=ru_doswin(RUSRP105,.FALSE.)
      FMTRP110=ru_doswin(RUSRP110,.FALSE.)
      
      VVOD=.TRUE.
      DUBL=.FALSE.
C================
      
      PRINT FMT100          
      PRINT FMT102
      READ(*, 200) REPLY
      IF(REPLY.EQ.'Y') DUBL=.TRUE.
      IF(.NOT.DUBL) GOTO 5
      PRINT FMT104
      READ(*, 200) REPLY
      WRITE(1, FMT100)
      WRITE(10, '(2a)') "������ ������������� *RP*",";"
      WRITE(1, FMT106)
      WRITE(10, '(2a)') "�������� ������:",";"
    5 PRINT FMT106
C*************���� ����������***
C      CALL RPSUB(T,X,V,PP)
      PRINT FMTRP100          
      READ(*,*) PP0
      !READ(2,*) PP0
      PRINT FMTRP105
      READ(*,*) PPX
      !READ(2,*) PPX
      PRINT FMTRP110
      READ(*,*) PPV
      !READ(2,*) PPV
      WRITE(1, FMTRP100) PP0
      WRITE(10, '(2a,/,2a,E13.5,a)') "�������� �������� :",";",
     &  "���������� ������������ (�):",";", PP0,";"
      WRITE(1, FMTRP105) PPX
      WRITE(10, '(2a,E13.5,a)') "����������� ��� X (�/�):",";", PPX,";"
      WRITE(1, FMTRP110) PPV
      WRITE(10, '(2a,E13.5,a)') "����������� ��� V (�*�/�):",";",PPV,";"
      
      
      PRINT FMT108
      READ(*, *) KA
      !READ(2,*) KA
      PRINT FMT110
      READ(*, *) R
      !READ(2,*) R    
      PRINT FMT112
      READ(*, *) TMP
      !READ(2,*) TMP
      PRINT FMT114
      READ(*, *) PM
      !READ(2,*) PM
      PRINT FMT116
      READ(*, *) PA
      !READ(2,*) PA
      PRINT FMT118
      READ(*, *) FE1
      !READ(2,*) FE1
      PRINT FMT120
      READ(*, *) FE2
      !READ(2,*) FE2
      PRINT FMT122
      READ(*, *) MS
      !READ(2,*) MS
      PRINT FMT124
      READ(*, *) D1
      !READ(2,*) D1
      PRINT FMT125
      READ(*, *) D2
      !READ(2,*) D2
      PRINT FMT126
      READ(*, *) D3
      !READ(2,*) D3
      PRINT FMT128
      READ(*, *) XMAX
      !READ(2,*) XMAX
      PRINT FMT130
      READ(*, *) X01
      !READ(2,*) X01
      PRINT FMT132
      READ(*, *) X02
      !READ(2,*) X02
      PRINT FMT134
      READ(*, *) TRP
      !READ(2,*) TRP
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
      PRINT 136, FP1,BTK,K1,K3,FP2,FIK,K2,KK
      IF(.NOT.DUBL) GO TO 7
C*************������ ����������***
      WRITE(1, FMT108) KA
      WRITE(10, '(2a,E13.5,a)') "���������� ��������:",";", KA,";"
      WRITE(1, FMT110) R
      WRITE(10, '(2a,E13.5,a)') "������� ���������� (��/��*�):",";",
     &R,";"
      WRITE(1, FMT112) TMP
      WRITE(10, '(2a,E13.5,a)') " ����������� � ���������� (���� �) :",
     &                          ";", TMP,";"
      WRITE(1, FMT114) PM
      WRITE(10, '(2a,E13.5,a)') "�������� � ���������� (���):",";",
     & PM,";"
      WRITE(1, FMT116) PA
      WRITE(10, '(2a,E13.5,a)') " �������� �� ������ (���):",";", PA,";"
      WRITE(1, FMT118) FE1
      WRITE(10, '(2a,E13.5,a)') " ���. ������� �� �����:",";", FE1,";"
      WRITE(1, FMT120) FE2
      WRITE(10, '(2a,E13.5,a)') " ���. ������� �� ������:",";", FE2,";"
      WRITE(1, FMT122) MS
      WRITE(10, '(2a,E13.5,a)') " ����� ������� (��):",";", MS,";"
      WRITE(1, FMT124) D1
      WRITE(10, '(2a,E13.5,a)') " ������� ������ (�):",";", D1,";"
      WRITE(1, FMT125) D2
      WRITE(10, '(2a,E13.5,a)') "������� ������� ����� (�):",";", D2,";"
      WRITE(1, FMT126) D3
      WRITE(10, '(2a,E13.5,a)') "������� ������� ����� (�):",";", D3,";"
      WRITE(1, FMT128) XMAX
      WRITE(10, '(2a,E13.5,a)') "������������ ��� (�):",";", XMAX,";"
      WRITE(1, FMT130) X01
      WRITE(10, '(2a,E13.5,a)') "�������� X01:",";", X01,";"
      WRITE(1, FMT132) X02
      WRITE(10, '(2a,E13.5,a)') "�������� X02:",";", X02,";"
      WRITE(1, FMT134) TRP
      WRITE(10, '(2a,E13.5,a)') "���� ������ �����:",";", TRP,";"
      WRITE(1, 136), FP1, BTK, K1, K3, FP2, FIK, K2, KK
C      WRITE(10, 136), FP1, BTK, K1, K3, FP2, FIK, K2, KK
    7 PM=(PM+0.1)*1E6
      PA=(PA+0.1)*1E6
      TMP=TMP+273.
C     ***
   10 IF(.NOT.DUBL) GO TO 20
      DO 15 I=1,6
   15 WRITE(1, 180)
      PRINT FMT104
      READ(*, 200) REPLY
   20 CONTINUE
C*************��������� ������*** 
      PRINT FMT140
      PRINT FMT142
      READ(*, *) X
      !READ(2,*) X
      PRINT FMT144
      READ(*, *) V
      !READ(2,*) V
      PRINT FMT146
      READ(*, *) P1
      !READ(2,*) P1
      PRINT FMT148
      READ(*, *) P2
      !READ(2,*) P2
      PRINT FMT150
      READ(*, *) DT
      !READ(2,*) DT
      PRINT FMT152
      READ(*, *) NSD
      !READ(2,*) NSD
      IF(NSD.LE.0) NSD=1
      IF(.NOT.DUBL) GO TO 25
      PRINT FMT154
      READ(*, *) NSP
      !READ(2,*) NSP
      IF(NSP.LE.0) NSP=1
      WRITE(1, FMT140)
      WRITE(10, '(a)') "��������� ������:"
      WRITE(1, FMT142) X
      WRITE(10, '(2a,E13.5,a)') "��� ������ (�):",";", X,";"
      WRITE(1, FMT144) V
      WRITE(10, '(2a,E13.5,a)') "�������� ������ (�):",";", V,";"
      WRITE(1, FMT146) P1
      WRITE(10, '(2a,E13.5,a)') "�������� � 1-� ������� (���):",";",
     & P1,";"
      WRITE(1, FMT148) P2
      WRITE(10, '(2a,E13.5,a)') "�������� �� 2-� ������� (���)",";",
     & P2,";"
      WRITE(1, FMT150) DT
      WRITE(10, '(2a,E13.5,a)') "��� �������������� (�):",";", DT,";"
      DO 22 I=1,6
   22 WRITE(1, 180)
      PRINT FMT104
      READ(*, 200) REPLY
      WRITE(1, FMT160)
      WRITE(10, '(16a)') "T (�)",";", "X (�)",";", "V (�/�)",";",
     & "P1(���)",";", "P2 (���)",";", "PP (��)",";", 
     & "G1 (���/�)",";", "G2 (���/�)",";"
      WRITE(1, 180)
C      WRITE(10, '(2a)') " ",";"
C*************�������� � ��*** 
   25 P1=(P1+0.1)*1E6
      P2=(P2+0.1)*1E6
      ISD=0
      ISP=0
      ISS=0
      T=0
      PRINT FMT160
      PRINT 180
      GO TO 40
C*************����� �����������***
   30 PRINT FMT160
      PRINT 180
   33 IF(ISD.NE.0) GO TO 35
      PRINT 162, T,X,V,P1OUT,P2OUT,PPOUT,G1,G2
      ISS=ISS+1
   35 IF(.NOT.DUBL) GO TO 40
      IF(ISP.NE.0) GO TO 40
      WRITE(1, 162) T,X,V,P1OUT,P2OUT,PPOUT,G1,G2
      WRITE(10, '(F9.3,a,5(F8.3,a),2(F10.5,a))') T,";",X,";",V,";",
     &        P1OUT,";",P2OUT,";",PPOUT,";",G1,";",G2,";"
C*************������ �����***     
   40 PP=PP0+PPX*X+PPV*V                 !CALL RPSUB(T,X,V,PP)
      PS=P1*FP1-P2*FP2-PP
      IF(V.EQ.0.) W=PS-SIGN(TRP,PS)
      IF(V.NE.0.) W=PS-SIGN(TRP,V)
      W=W/MS
C*************������� ����� ������������*** 
      IF(ABS(PS).GT.TRP) GO TO 55
      DV=W*DT
      IF(DV*V.LT.0. .AND. ABS(DV).LT.ABS(V)) GO TO 55
C*************������� � ����� ��-�� ������*** 
      V=0.
      W=0.
C***      
   55 Y(1)=X
      DY(1)=V
      Y(2)=V
      DY(2)=W
      IF(P1.GT.PM) GO TO 65      
C*************1-� ������� ��������*** 
      BT=P1/PM
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G1=FE1*KK*PM*FI/SQRT(R*TMP)
      DY(3)=KA*R*TMP*G1-KA*P1*FP1*V
      DY(3)=DY(3)/(FP1*(X01+X))
      Y(3)=P1
      GO TO 70
C*************1-� ������� ���������*** 
   65 BT=PM/P1
      TMP1=TMP*(P1/PM)**K3
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G1=-FE1*KK*P1*FI/SQRT(R*TMP1)
      DY(3)=-KA*R*TMP1*(-G1)-KA*P2*FP2*V
      DY(3)=DY(3)/(FP1*(X01+X))
      Y(3)=P1
C*** 
   70 IF(P2.GT.PA) GO TO 75
C*************2-� ������� ��������*** 
      BT=P2/PA
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G2=FE2*KK*PA*FI/SQRT(R*TMP)
      DY(4)=KA*R*TMP*G2+KA*P2*FP2*V
      DY(4)=DY(4)/(FP2*(X02+XMAX-X))
      Y(4)=P2
      GO TO 80
C*************2-� ������� ���������*** 
   75 BT=PA/P2
      TMP2=TMP*(P2/PM)**K3
      FI=FIK
      IF(BT.GT.BTK) FI=SQRT(BT**K1-BT**K2)
      G2=-FE2*KK*P2*FI/SQRT(R*TMP2)
      DY(4)=-KA*R*TMP2*(-G2)+KA*P2*FP2*V
      DY(4)=DY(4)/(FP2*(X02+XMAX-X))
      Y(4)=P2      
C*************��� �������������� ������� ������*** 
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
      PPOUT=PP*1.E-3
      PSOUT=PS*1.E-3      
      IF(X.GT.0.) GO TO 85      
C*************����������� ���� �����*** 
      X=0.
      IF(V.LT.0.) V=0.
      GO TO 90
   85 IF(X.LT.XMAX) GO TO 90    
C*************����������� ���� ������*** 
      X=XMAX
      IF(V.GT.0.) V=0.
C***
   90 ISD=ISD+1
      ISP=ISP+1
      IF(ISD.GE.NSD) ISD=0
      IF(ISP.GE.NSP) ISP=0
      IF(ISS.LT.20) GO TO 33
      ISS=0
      PRINT FMT166
      READ(*, 200) REPLY
      IF(REPLY.EQ.'E') STOP     !RETURN
      IF(REPLY.NE. 'N') GO TO 30
      PP=PP0+PPX*X+PPV*V !GO TO 10 !!!!!!!!!!!!!
      CLOSE(1)
      CLOSE(2)
      CLOSE(10)
      END
      
      SUBROUTINE RPSUB(T,X,V,PP)
      use text_transfer
      REAL,INTENT(IN)::T,X,V
      REAL,INTENT(OUT)::PP
      LOGICAL VVOD,DUBL
      CHARACTER(100) RUSRP100,RUSRP105,RUSRP110
      CHARACTER(100) FMTRP100,FMTRP105,FMTRP110 
      COMMON /VD/ VVOD,DOUBLE
  100 FORMAT(/10X, '�������� �������� :',  
     &       /10X, '���������� ������������ (�) :', E13.5)
  105 FORMAT(/10X, '����������� ��� X      (�/�):',  E13.5)
  110 FORMAT(/10X, '����������� ��� V      (�*�/�):',  E13.5)
      RUSRP100="(/10X, '�������� �������� :',  
     &       /10X, '���������� ������������ (�) :', E13.5)"      
      RUSRP105="(/10X, '����������� ��� X      (�/�):',  E13.5)"
      RUSRP110="(/10X, '����������� ��� V      (�*�/�):',  E13.5)"
      FMTRP100=ru_doswin(RUSRP100,.FALSE.)
      FMTRP105=ru_doswin(RUSRP105,.FALSE.)
      FMTRP110=ru_doswin(RUSRP110,.FALSE.)
      IF(.NOT.VVOD) GO TO 10
C*************����***
      PRINT FMTRP100          
      READ(*,*) PP0
      !READ(2,*) PP0
      PRINT FMTRP105
      READ(*,*) PPX
      !READ(2,*) PPX
      PRINT FMTRP110
      READ(*,*) PPV
      !READ(2,*) PPV
      IF(.NOT.DUBL) RETURN
      WRITE(1, FMTRP100) PP0
      WRITE(1, FMTRP105) PPX
      WRITE(1, FMTRP110) PPV
C*************������***
   10 PP=PP0+PPX*X+PPV*V
      RETURN
      END
      
     
      