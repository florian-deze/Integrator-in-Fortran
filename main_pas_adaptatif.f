      PROGRAM MAIN_PAS_ADAPTATIF
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
      DOUBLE PRECISION ERROR
      INTEGER J
      DOUBLE PRECISION EPSLN, HMIN
      EXTERNAL PROBLEM_F
*
* Debut du programme principal
*

      CALL PROBLEM_INIT (N, X0, XEND, Y0)
* Ici, les variables du problème sont initialisées
      CALL SCHEMA_INIT (S, P, PHAT, C, A, SMAX, B, BHAT)
      EPSLN = 1D-5
      HMIN = 1D-3
* Ici, les variables du schema sont initialisées
      WRITE (*,*) X0, (Y0(J), J = 1,N)

* Appel de l'integrateur 
      CALL RK_ADAPTATIF (N, X0, XEND, Y0, PROBLEM_F,
     $                   S, P, PHAT, C, A, SMAX, B, BHAT, NMAX,
     $                   HMIN, EPSLN)


* calcul de l'erreur
      CALL PROBLEM_ERR (N, Y0, ERROR)
      IF (ERROR .GE. 0D0) THEN
          WRITE (*,*) '# Erreur relative globale = ', ERROR
      ELSE
           WRITE (*,*) '# Erreur relative globale inconnue'
      END IF
      END PROGRAM

      

      
      SUBROUTINE RK_ADAPTATIF (N, X0, XEND, Y0, FCN,
     $                   S, P, PHAT, C, A, SMAX, B, BHAT, NMAX,
     $                   HMIN, EPSLN)
      IMPLICIT NONE
      
      INTEGER N,NBSTEPS,NBCALCULPAS
      DOUBLE PRECISION X0, XEND, Y0(N)

      INTEGER S, P, PHAT, SMAX, NMAX, I, J
      DOUBLE PRECISION C(S),Q
      DOUBLE PRECISION A(SMAX,SMAX)
      DOUBLE PRECISION B(SMAX), BHAT(SMAX)
     
      DOUBLE PRECISION HMIN, EPSLN
      
      DOUBLE PRECISION H, HNEW, ERREUR      
      DOUBLE PRECISION Y1(NMAX), Y1HAT(NMAX)
      EXTERNAL FCN

* Q est le min entre l'ordre de la formule 
* et celui de la formule emboitee
      Q = MIN(PHAT,P)
* NBSTEPS sert a connaitre le nombre de pas utilise a la fin du programme
      NBSTEPS=0
      NBCALCULPAS=0
      H = MIN(XEND-X0,HMIN)

      DO WHILE (X0 .LT. XEND)
* Calculer Y1 et Y1HAT a partir de Y0
         CALL CALCUL_Y1_Y1HAT (A, SMAX, B, BHAT, C, S,
     $                            N, X0, H, Y0,
     $                            Y1, Y1HAT)


* Calculer ERREUR et HNEW
         ERREUR = 0D0
         DO I = 1,N
             ERREUR = ERREUR + ((Y1(I)-Y1HAT(I)) /
     $                 (1D0 + MAX(ABS(Y0(I)),ABS(Y1(I)))))**2
         END DO
         ERREUR = SQRT(ERREUR/DBLE(N))
         HNEW = H * MIN(5D0,MAX(1D0/5D0,9D-1 *
     $                (EPSLN/ERREUR)**(1/(1+Q))))

         NBCALCULPAS =NBCALCULPAS+2
         IF (ERREUR .LE. EPSLN) THEN
* le pas est accepte
             X0 = X0 + H
             NBSTEPS = NBSTEPS+1
             DO I = 1,N
                 Y0(I) = Y1(I)
             END DO
             H = MIN(HNEW,XEND-X0)
         ELSE
* le pas est rejete
             H = HNEW
         END IF
* affichage des valeurs calculees
         WRITE (*,*) X0, (Y0(J), J = 1,N)
      END DO
* affichage du nombre de pas utilises
      WRITE(*,*)'Nombre de pas',NBSTEPS
* affichage du nombre de calcul de l erreur et du nouveau pas
      WRITE(*,*)'Nombre de calculs erreur+Hnew',NBCALCULPAS
      END SUBROUTINE


      SUBROUTINE CALCUL_Y1_Y1HAT (A, LDA, B, BHAT, C, S, 
     $                            N, X0, H, Y0, 
     $                            Y1, Y1HAT)
* Y1 et Y1HAT sont en mode resultat.
* Les autres paramÃ‹tres formels sont en mode donnÃˆe
      INTEGER LDA, N, S
      DOUBLE PRECISION A(LDA,*), B(*), BHAT(*), C(*)
      DOUBLE PRECISION X0, H, Y0(*)
      DOUBLE PRECISION Y1(*), Y1HAT(*)
*
* Variables locales
*
* Chaque colonne de la matrice K correspond â€¡ un des vecteurs
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
      DO I=1,N
         Y1HAT(I)=Y0(I)
         DO J=1,S
           Y1HAT(I)=Y1HAT(I)+H*BHAT(J)*K(I,J)
         END DO
      END DO

      END SUBROUTINE
