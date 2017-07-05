* Le schéma de RK4 (sch_RK4.f)

      SUBROUTINE SCHEMA_INIT (S, P, PHAT, C, A, LDA, B, BHAT)
*
* Les coefficients qui définissent le schéma de RK4
*
* S     Résultat. INTEGER. Le nombre d'étages
* P     Résultat. INTEGER. L'ordre de la formule principale
* PHAT  Résultat. INTEGER. L'ordre de la formule emboîtée.
* C     Résultat. DOUBLE PRECISION C(S). Les coefficients c_i du schéma
* A     Résultat. DOUBLE PRECISION A(S,S). Les coefficients a_{ij} du schéma
* LDA   Donnée.   INTEGER. La leading dimension de A, telle qu'elle a 
*       été déclarée. Même convention qu'en LAPACK.
* B     Résultat. DOUBLE PRECISION B(S). Les coefficients b_j du schéma
* BHAT  Résultat. DOUBLE PRECISION BHAT(S). Les coefficients bhat_j du
*       schéma, pour la formule emboîtée.
      INTEGER LDA, S, P, PHAT
      DOUBLE PRECISION A(LDA,*), B(*), BHAT(*), C(*)
*
      S = 4
      P = 4
      C(1) = 0D0
      C(2) = (1D0/2D0)
      C(3) = (1D0/2D0)
      C(4) = 1D0
      A(2,1) = (1D0/2D0)
      A(3,1) = 0D0
      A(3,2) = 1D0/2D0
      A(4,1) = 0D0
      A(4,2) = 0D0
      A(4,3) = 1D0
      B(1) = 1D0/6D0
      B(2) = 1D0/3D0
      B(3) = 1D0/3D0
      B(4) = 1D0/6D0
* Formule emboîtée : la méthode d'Euler
      PHAT = 1
      BHAT(1) = 1D0
      BHAT(2) = 0D0
      END SUBROUTINE

