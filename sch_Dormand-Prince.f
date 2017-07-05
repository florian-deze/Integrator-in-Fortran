      SUBROUTINE SCHEMA_INIT (S, P, PHAT, C, A, LDA, B, BHAT)
*
* Les coefficients qui définissent Dormand-Prince 5(4)
*
* S     Résultat. INTEGER. Le nombre d'étages
* P     Résultat. INTEGER. L'ordre de la formule principale
* PHAT  Résultat. INTEGER. L'ordre de la formule emboîtée.
* C     Résultat. DOUBLE PRECISION C(S). Les coefficients c_i du schéma
* A     Résultat. DOUBLE PRECISION A(S,S). Les coefficients a_{ij} du schéma
* LDA   Donnée. INTEGER. La leading dimension de A, telle qu'elle a 
*       été déclarée. Même convention qu'en LAPACK.
* B     Résultat. DOUBLE PRECISION B(S). Les coefficients b_j du schéma
* BHAT  Résultat. DOUBLE PRECISION BHAT(S). Les coefficients bhat_j du
*       schéma, pour la formule emboîtée.
*
      IMPLICIT NONE
      INTEGER S, P, PHAT, LDA
      DOUBLE PRECISION C(*)
      DOUBLE PRECISION A(LDA,*)
      DOUBLE PRECISION B(*), BHAT(*)
*
      S = 7
      P = 5
      PHAT = 4
*
      C(1) = 0D0
      C(2) = 1D0/5D0
      C(3) = 3D0/10D0
      C(4) = 4D0/5D0
      C(5) = 8D0/9D0
      C(6) = 1D0
      C(7) = 1D0
      A(2,1) = 1D0/5D0
      A(3,1) = 3D0/40D0
      A(3,2) = 9D0/40D0
      A(4,1) = 44D0/45D0
      A(4,2) = -56D0/15D0
      A(4,3) = 32D0/9D0
      A(5,1) = 19372D0/6561D0
      A(5,2) = -25360D0/2187D0
      A(5,3) = 64448D0/6561D0
      A(5,4) = -212D0/729D0
      A(6,1) = 9017D0/3168D0
      A(6,2) = -355D0/33D0
      A(6,3) = 46732D0/5247D0
      A(6,4) = 49D0/176D0
      A(6,5) = -5103D0/18656D0
      A(7,1) = 35D0/384D0
      A(7,2) = 0D0
      A(7,3) = 500D0/1113D0
      A(7,4) = 125D0/192D0
      A(7,5) = -2187D0/6784D0
      A(7,6) = 11D0/84D0
      B(1) = 35D0/384D0
      B(2) = 0D0
      B(3) = 500D0/1113D0
      B(4) = 125D0/192D0
      B(5) = -2187D0/6784D0
      B(6) = 11D0/84D0
      B(7) = 0D0
* Formule emboîtée
      BHAT(1) = 5179D0/57600D0
      BHAT(2) = 0D0
      BHAT(3) = 7571D0/16695D0
      BHAT(4) = 393D0/640D0
      BHAT(5) = -92097D0/339200D0
      BHAT(6) = 187D0/2100D0
      BHAT(7) = 1D0/40D0
      END SUBROUTINE
