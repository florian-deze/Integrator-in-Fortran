* Le problème de l'exponentielle

      SUBROUTINE PROBLEM_INIT (N, X0, XEND, Y0)
*
* N     Résultat. INTEGER. La dimension de Y0
* X0    Résultat. DOUBLE PRECISION. Borne inf. de l'intervalle d'intégration
* XEND  Résultat. DOUBLE PRECISION. Borne sup. de l'intervalle d'intégration
* Y0    Résultat. DOUBLE PRECISION Y0(N). Les conditions initiales
*
      INTEGER N
      DOUBLE PRECISION X0, XEND, Y0(*)
*
      X0 = 0D0
      XEND = 1D0
      Y0(1) = 1D0
      N = 1
      END SUBROUTINE

      SUBROUTINE PROBLEM_F (N, X, Y, F)
      INTEGER N
      DOUBLE PRECISION X, Y(*), F(*)
*
* N     Donnée.   INTEGER. La dimension de Y et de F
* X     Donnée.   DOUBLE PRECISION. La variable indépendante
* Y     Donnée.   DOUBLE PRECISION Y(N). Les valeurs des variables dépendantes
* F     Résultat. DOUBLE PRECISION F(N). Les vitesses des variables dépendantes
*
      F(1) = Y(1)
      END SUBROUTINE

      SUBROUTINE PROBLEM_ERR (N, Y, ERROR)
*
* N     Donnée.   INTEGER. La dimension de Y
* Y     Donnée.   DOUBLE PRECISION Y(N). Les variables dépendantes en X = XEND
* ERROR Résultat. DOUBLE PRECISION. L'erreur relative globale. -1 si inconnue
*
      INTEGER N
      DOUBLE PRECISION Y(*), ERROR
*
      ERROR = ABS(EXP(1D0) - Y(1))/EXP(1D0)
      END SUBROUTINE

