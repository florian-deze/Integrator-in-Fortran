      PROGRAM MAIN_PAS_FIXE
      IMPLICIT NONE
*
* Les variables du schéma
*
      INTEGER SMAX
      PARAMETER (SMAX = 10)
      DOUBLE PRECISION A(SMAX,SMAX), B(SMAX), BHAT(SMAX), C(SMAX)
      INTEGER S, P, PHAT
*
* Les variables du problème
*
      INTEGER NMAX
      PARAMETER (NMAX = 10)
      DOUBLE PRECISION Y0(NMAX)
      DOUBLE PRECISION X0, XEND
      INTEGER N
*
* Les variables de l'intégrateur
*
      DOUBLE PRECISION Y1(NMAX), Y1HAT(NMAX), ERROR
      INTEGER NBSTEPS
      INTEGER J,I,TOT
      DOUBLE PRECISION H
*
* Instructions
*
      WRITE (*,*) '#     NBSTEPS   NB BITS EXACTS (APPROX.)'
      CALL SCHEMA_INIT (S, P, PHAT, C, A, SMAX, B, BHAT)
* Ici, les variables du schéma sont initialisées
* Ici, les variables du problème sont initialisées
      TOT = 100000
      NBSTEPS=4
      DO WHILE (NBSTEPS.LE.TOT) 
        CALL PROBLEM_INIT (N, X0, XEND, Y0)
        H = (XEND - X0)/NBSTEPS
        DO I=1,N
          Y1(I)=0D0
          Y1HAT(I)=0D0
        END DO
* Boucle principale qui parcourt tout l'intervalle d'intégration
        DO I=1,NBSTEPS
* Calculer Y1 et Y1HAT à partir de Y0
           CALL CALCUL_Y1_Y1HAT (A, SMAX, B, BHAT, C, S,
     $                            N, X0, H, Y0,
     $                            Y1, Y1HAT)
* Avancer : X0 = X0+H, Y0 = Y1
           X0 = X0 + H
           DO J = 1,N
              Y0(J) = Y1(J)
           END DO
* Afficher
*         WRITE (*,*) X0, (Y0(J), J = 1,N)
        END DO
      
        CALL PROBLEM_ERR (N, Y0, ERROR)
        IF (ERROR .GE. 0D0) THEN
           ERROR=-LOG(ERROR)/LOG(2D0)

           WRITE (*,*) '#',NBSTEPS,ERROR
        ELSE
           WRITE (*,*) '# Erreur relative globale inconnue'
        END IF
        NBSTEPS=NBSTEPS*2
      END DO
      END PROGRAM

      SUBROUTINE CALCUL_Y1_Y1HAT (A, LDA, B, BHAT, C, S, 
     $                            N, X0, H, Y0, 
     $                            Y1, Y1HAT)
* Y1 et Y1HAT sont en mode résultat.
* Les autres paramètres formels sont en mode donnée
      INTEGER LDA, N, S
      DOUBLE PRECISION A(LDA,*), B(*), BHAT(*), C(*)
      DOUBLE PRECISION X0, H, Y0(*)
      DOUBLE PRECISION Y1(*), Y1HAT(*)
*
* Variables locales
*
* Chaque colonne de la matrice K correspond à un des vecteurs
* k1, k2, ..., ks des tableaux de Butcher.
      DOUBLE PRECISION K(N,S)
      DOUBLE PRECISION YTEMP(N)
      INTEGER I,J,L
*calcul des k(N,S)
       CALL PROBLEM_F(N,X0,Y0,K(1,1))
       DO J=2,S
          DO I=1,N
          YTEMP(I) = Y0(I)
           DO L=1,J-1
            YTEMP(I)=YTEMP(I)+ A(J,L)*K(I,L)*H
           END DO
          CALL PROBLEM_F(N, X0+H*C(J), YTEMP,K(1,J))
          END DO
       END DO
* Pour I de 1 a N et J de 1 a S
* y1(I) = y0 + h*(b1*k1 + b2*k2 + ... + bs*ks)
* 
      DO I=1,N
         Y1(I)=Y0(I)
         DO J=1,S
           Y1(I)=Y1(I)+H*B(J)*K(I,J)
         END DO
      END DO
* Pour I de 1 a N et J de 1 a S
* y1hat = y0 + h*(bhat1*k1 + bhat2*k2 + ... + bhats*ks)
*
      DO I = 1,N
         Y1HAT(I)=Y0(I)
         DO J=1,S
            Y1HAT(I) = Y1HAT(I) + H*BHAT(J)*K(I,J)
         END DO
      END DO
      END SUBROUTINE

