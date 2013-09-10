C
C     FFTE: A FAST FOURIER TRANSFORM PACKAGE
C
C     (C) COPYRIGHT SOFTWARE, 2000-2004, 2008-2011, ALL RIGHTS RESERVED
C                BY
C         DAISUKE TAKAHASHI
C         FACULTY OF ENGINEERING, INFORMATION AND SYSTEMS
C         UNIVERSITY OF TSUKUBA
C         1-1-1 TENNODAI, TSUKUBA, IBARAKI 305-8573, JAPAN
C         E-MAIL: daisuke@cs.tsukuba.ac.jp
C
C
C     2-D COMPLEX FFT ROUTINE (FOR VECTOR MACHINES)
C
C     FORTRAN90 SOURCE PROGRAM
C
C     CALL ZFFT2D(A,NX,NY,IOPT)
C
C     A(NX,NY) IS COMPLEX INPUT/OUTPUT VECTOR (COMPLEX*16)
C     NX IS THE LENGTH OF THE TRANSFORMS IN THE X-DIRECTION (INTEGER*4)
C     NY IS THE LENGTH OF THE TRANSFORMS IN THE Y-DIRECTION (INTEGER*4)
C       ------------------------------------
C         NX = (2**IP) * (3**IQ) * (5**IR)
C         NY = (2**JP) * (3**JQ) * (5**JR)
C       ------------------------------------
C     IOPT = 0 FOR INITIALIZING THE COEFFICIENTS (INTEGER*4)
C          = -1 FOR FORWARD TRANSFORM
C          = +1 FOR INVERSE TRANSFORM
C
C     WRITTEN BY DAISUKE TAKAHASHI
C
      SUBROUTINE ZFFT2D(A,NX,NY,IOPT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'param.h'
      COMPLEX*16 A(*)
      COMPLEX*16 WX(NDA2),WY(NDA2)
      COMPLEX*16 B(:)
      ALLOCATABLE :: B
!DIR$ ATTRIBUTES ALIGN : 16 :: B
      DIMENSION LNX(3),LNY(3)
      SAVE WX,WY
C
      CALL FACTOR(NX,LNX)
      CALL FACTOR(NY,LNY)
C
      IF (IOPT .EQ. 0) THEN
        CALL SETTBL(WX,NX)
        CALL SETTBL(WY,NY)
        RETURN
      END IF
C
      IF (IOPT .EQ. 1) THEN
!DIR$ VECTOR ALIGNED
        DO 10 I=1,NX*NY
          A(I)=DCONJG(A(I))
   10   CONTINUE
      END IF
C
      ALLOCATE(B(NX*NY))
      CALL MFFT235A(A,B,WY,NX,NY,LNY)
      CALL ZTRANS(A,B,NX,NY)
      CALL MFFT235A(B,A,WX,NY,NX,LNX)
      CALL ZTRANS(B,A,NY,NX)
      DEALLOCATE(B)
C
      IF (IOPT .EQ. 1) THEN
        DN=1.0D0/(DBLE(NX)*DBLE(NY))
!DIR$ VECTOR ALIGNED
        DO 20 I=1,NX*NY
          A(I)=DCONJG(A(I))*DN
   20   CONTINUE
      END IF
      RETURN
      END
