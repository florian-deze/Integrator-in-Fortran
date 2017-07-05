      SUBROUTINE PROBLEM_INIT (N, X0, XEND, Y0)
*
* Des conditions initiales pour Arenstorf (Hairer, Norsett, Wanner, p. 130)
*
* Y = (y1, y2, y1', y2')
*
* N     Résultat. INTEGER. La dimension de Y0
* X0    Résultat. DOUBLE PRECISION. Borne inf. de l'intervalle d'intégration
* XEND  Résultat. DOUBLE PRECISION. Borne sup. de l'intervalle d'intégration
* Y0    Résultat. DOUBLE PRECISION Y0(N). Les conditions initiales
*
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION X0, XEND, Y0(*)
*
      X0 = 0D0
      XEND = 17.0652165601579625588917206249
      N = 4
      Y0(1) = 0.994D0
      Y0(2) = 0D0
      Y0(3) = 0D0
      Y0(4) = -2.00158510637908252240537862224D0
      END SUBROUTINE

      SUBROUTINE PROBLEM_F (N, X, Y, F)
*
* Une fonction R x R^N -> R^n
*              (X, Y)  -> F = FCN (X, Y)
*
* Les vecteurs Y et F sont de dimension N = 4
* Il s'agit d'un système dans le plan (y1, y2) d'ordre 2.
*
* Y = (y1, y2, y1', y2')
*
* Voir Hairer, Norsett, Wanner, p. 476 pour le code FORTRAN
* et p. 129 pour le système mathématique
*
* N     Donnée.   INTEGER. La dimension de Y et de F
* X     Donnée.   DOUBLE PRECISION. La variable indépendante
* Y     Donnée.   DOUBLE PRECISION Y(N). Les valeurs des variables dépendantes
* F     Résultat. DOUBLE PRECISION F(N). Les vitesses des variables dépendantes
*
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION X, Y(N), F(N)
*
      DOUBLE PRECISION MU, MUP, R1, R2
      PARAMETER (MU = 0.012277471D0)
      PARAMETER (MUP = 1D0 - MU)
      F(1) = Y(3)
      F(2) = Y(4)
      R1 = (Y(1) + MU)**2+Y(2)**2
      R1 = R1 * SQRT(R1)
      R2 = (Y(1) - MUP)**2+Y(2)**2
      R2 = R2 * SQRT(R2)
      F(3) = Y(1)+2D0*Y(4)-MUP*(Y(1)+MU)/R1-MU*(Y(1)-MUP)/R2
      F(4) = Y(2)-2D0*Y(3)-MUP*Y(2)/R1-MU*Y(2)/R2
      END SUBROUTINE

      SUBROUTINE PROBLEM_ERR (N, Y, ERROR)
*
* Erreur finale pour Arenstorf (on triche)
*
* N     Donnée.   INTEGER. La dimension de Y
* Y     Donnée.   DOUBLE PRECISION Y(N). Les variables dépendantes en X = XEND
* ERROR Résultat. DOUBLE PRECISION. L'erreur relative globale (-1 si inconnue)
*
      INTEGER N
      DOUBLE PRECISION Y(N), ERROR
*
      DOUBLE PRECISION NRM0
      NRM0 = 0.994D0
      ERROR = SQRT ((Y(1) - 0.994D0)**2 + Y(2)**2) / NRM0
      END SUBROUTINE

