* Le sch�ma de RK4 (sch_RK4.f)

      SUBROUTINE SCHEMA_INIT (S, P, PHAT, C, A, LDA, B, BHAT)
*
* Les coefficients qui d�finissent le sch�ma de RK4
*
* S     R�sultat. INTEGER. Le nombre d'�tages
* P     R�sultat. INTEGER. L'ordre de la formule principale
* PHAT  R�sultat. INTEGER. L'ordre de la formule embo�t�e.
* C     R�sultat. DOUBLE PRECISION C(S). Les coefficients c_i du sch�ma
* A     R�sultat. DOUBLE PRECISION A(S,S). Les coefficients a_{ij} du sch�ma
* LDA   Donn�e.   INTEGER. La leading dimension de A, telle qu'elle a 
*       �t� d�clar�e. M�me convention qu'en LAPACK.
* B     R�sultat. DOUBLE PRECISION B(S). Les coefficients b_j du sch�ma
* BHAT  R�sultat. DOUBLE PRECISION BHAT(S). Les coefficients bhat_j du
*       sch�ma, pour la formule embo�t�e.
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
* Formule embo�t�e : la m�thode d'Euler
      PHAT = 1
      BHAT(1) = 1D0
      BHAT(2) = 0D0
      END SUBROUTINE

