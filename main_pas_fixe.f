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
      INTEGER I, J
      DOUBLE PRECISION H
*
* Instructions
*
      CALL SCHEMA_INIT (S, P, PHAT, C, A, SMAX, B, BHAT)
* Ici, les variables du schéma sont initialisées
      CALL PROBLEM_INIT (N, X0, XEND, Y0)
* Ici, les variables du problème sont initialisées
      NBSTEPS = 40000
      H = (XEND - X0)/NBSTEPS
* Boucle principale qui parcourt tout l'intervalle d'intégration
      DO I = 1,NBSTEPS
* Calculer Y1 et Y1HAT à partir de Y0
         CALL CALCUL_Y1_Y1HAT (A, SMAX, B, BHAT, C, S,
     $                            N, X0, H, Y0,
     $                            Y1, Y1HAT)
* Avancer : X0 = X0+H, Y0 = Y1
         X0 = X0 + H
         DO J = 1,N
            Y0(J) = Y1(J)
         END DO
      END DO
* Afficher
      WRITE (*,*) X0, (Y0(J), J = 1,N)

      CALL PROBLEM_ERR (N, Y0, ERROR)
      IF (ERROR .GE. 0D0) THEN
          WRITE (*,*) '# Erreur relative globale = ', ERROR
      ELSE
           WRITE (*,*) '# Erreur relative globale inconnue'
      END IF
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
      INTEGER I
*
* k1 = f(x0,y0)
* Ici, K(1,1) = l'adresse de la première colonne de K.
*
      CALL PROBLEM_F (N, X0, Y0, K(1,1))
*
* Le code est spécialisé pour le schéma de Runge.
* k2 = f(x0 + h/2, y0 + h/2*k1) 
*
      DO I = 1,N
         YTEMP(I) = Y0(I) + A(2,1)*H*K(I,1)
      END DO
* Ici, K(1,2) = l'adresse de la deuxième colonne de K.
      CALL PROBLEM_F (N, X0 + H*C(2), YTEMP, K(1,2))

*
*k2 = f(x0+c3*h, y0+h(A(3,1)*K(1,1))
*
      DO I=1,N
         YTEMP(I)= Y0(I) + A(3,1)*H*K(I,1) + A(3,2)*H*K(I,2)
      END DO

* Ici, K(1,3) = l'adresse de la troisième colonne de K.
      CALL PROBLEM_F (N, X0 + H*C(3), YTEMP, K(1,3))


*
* y1 = y0 + h*(b1*k1 + b2*k2 + b3*k3)
*
      DO I = 1,N
         Y1(I) = Y0(I) + H*(B(1)*K(I,1) + B(2)*K(I,2) + B(3)*K(I,3))
      END DO
*
* y1hat = y0 + h*(bhat1*k1 + bhat2*k2 + bhat3*k3)
*
      DO I = 1,N
         Y1HAT(I) = Y0(I) + H*(BHAT(1)*K(I,1) + BHAT(2)*K(I,2) 
     $              + BHAT(3)*K(I,3))
      END DO
      END SUBROUTINE

