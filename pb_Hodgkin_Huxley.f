      SUBROUTINE PROBLEM_INIT (N, X0, XEND, Y0)
*
* Des conditions initiales pour Hodgkin-Huxley
*
* Y = (y1, y2, y1', y2')
*
* N     R?sultat. INTEGER. La dimension de Y0
* X0    R?sultat. DOUBLE PRECISION. Borne inf. de l'intervalle d'int?gration
* XEND  R?sultat. DOUBLE PRECISION. Borne sup. de l'intervalle d'int?gration
* Y0    R?sultat. DOUBLE PRECISION Y0(N). Les conditions initiales
*
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION X0, XEND, Y0(*)
*
      X0 = 0D0
      XEND = 60D0
      N = 4
      Y0(1) = -65D0
      Y0(2) = 1D0/3D0
      Y0(3) = 0D0
      Y0(4) = 2D0/3D0
      END SUBROUTINE


      SUBROUTINE PROBLEM_F (N, X, Y, F)
      INTEGER N
      DOUBLE PRECISION X, Y(*), F(*)
*
* N     Donn?e.   INTEGER. La dimension de Y et de F
* X     Donn?e.   DOUBLE PRECISION. La variable ind?pendante
* Y     Donn?e.   DOUBLE PRECISION Y(N). Les valeurs des variables d?pendantes
* F     R?sultat. DOUBLE PRECISION F(N). Les vitesses des variables d?pendantes
*

      DOUBLE PRECISION GK, GNA, GL, VK, VNA, VL, IAPP
      DOUBLE PRECISION ALPHAN, BETAN, ALPHAM, BETAM, ALPHAH, BETAH

* Initialisation des constantes de Hodgkin-Huxley
      GK = 36D0
      GNA = 12D1
      GL = 3D-1
      VK = -77D0
      VNA = 5D1
      VL = -544D-1
      IAPP = 25D0/(1D0+EXP(2D1-X))
      N=4

* calcul

      F(1) = -GK*(Y(2)**4)*(Y(1)-VK)-GNA*(Y(3)**3)*Y(4)*(Y(1)-VNA)-
     $       GL*(Y(1)-VL)+IAPP
*
      ALPHAN = (1D-2)*(-Y(1)-55D0)/(EXP((-Y(1)-55D0)/1D1)-1D0)
      BETAN = (125D-3)*EXP((-Y(1)-65D0)/80D0)
      F(2) = ALPHAN*(1D0-Y(2))-BETAN*Y(2)
*
      ALPHAM = (1D-1)*(-Y(1)-40D0)/(EXP((-Y(1)-40D0)/1D1)-1D0)
      BETAM = 4D0*EXP((-Y(1)-65D0)/18D0)
      F(3) = ALPHAM*(1D0-Y(3))-BETAM*Y(3)
*
      ALPHAH = (7D-2)*EXP((-Y(1)-65D0)/2D1)
      BETAH = 1D0/(1D0+EXP((-Y(1)-35D0)/1D1))
      F(4) = ALPHAH*(1D0-Y(4))-BETAH*Y(4)
*
      END SUBROUTINE

      SUBROUTINE PROBLEM_ERR (N, Y, ERROR)
*
* N     Donn?e.   INTEGER. La dimension de Y
* Y     Donn?e.   DOUBLE PRECISION Y(N). Les variables d?pendantes en X = XEND
* ERROR R?sultat. DOUBLE PRECISION. L'erreur relative globale. -1 si inconnue
*
      INTEGER N
      DOUBLE PRECISION Y(*), ERROR
* On ne peut pas calcule l erreur de ce probleme
* On pose une erreur factice
      ERROR = 1D-5
      END SUBROUTINE

