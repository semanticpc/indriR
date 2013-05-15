#include "utils.h"
using namespace Rcpp;
using namespace std;

class GreedyDiversity {
public:

private:
    arma::mat similarityMatrix;
    arma::colvec relScores;
public:

    GreedyDiversity(arma::mat _simMat, arma::vec _relScores) {
        similarityMatrix = _simMat;
        relScores = _relScores.t();
    }

    SEXP MMR(double lambda){

    }

    SEXP Zhai(double lambda){

    }

    SEXP SetBased(double lambda){

    }




};

RCPP_MODULE(GreedyDiversity) {
    using namespace Rcpp;

    class_<GreedyDiversity > ("GreedyDiversity")
            .constructor<arma::mat, arma::vec>("Default Constructor")
            .method("MMR", &GreedyDiversity::MMR, "Test Documentation");

            // read-only property;
}
