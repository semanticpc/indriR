#include "utils.h"
using namespace Rcpp;
using namespace std;

class ScoringFeatures {
public:

private:
    arma::mat termDocMat;
    arma::rowvec idf;
    arma::rowvec ctf;
    arma::rowvec df;
    vector<string> extDocumentIDs;
public:

    ScoringFeatures(arma::mat _termDocMatrix, arma::mat _termStats,
                    vector<string> _docIDs) {
        termDocMat = _termDocMatrix;
        df = _termStats.col(0).t();
        idf = _termStats.col(1).t();
        ctf = _termStats.col(2).t();
        extDocumentIDs = _docIDs;

    }

    SEXP getSimilarityMatrix(){
        arma::mat tfidfMat = termDocMat;
        tfidfMat.each_row() %= idf;
        arma::mat cosSim = arma::zeros(termDocMat.n_rows, termDocMat.n_rows);
        for(int i = 0; i < termDocMat.n_rows; i++){
            double i_mod = arma::sum(arma::square(tfidfMat.row(i)));
            for(int j = i; j < termDocMat.n_rows; j++){
                double j_mod = arma::sum(arma::square(tfidfMat.row(j)));
                cosSim(i,j) = arma::dot(tfidfMat.row(i), tfidfMat.row(j)) /
                                                     (std::sqrt(i_mod * j_mod));

            }
        }
        NumericMatrix d = wrap(cosSim);
        List dimnms = Rcpp::List::create(extDocumentIDs, extDocumentIDs);
        d.attr("dimnames") = dimnms;
        return d;
    }

    SEXP BM25(){

    }

    SEXP LM(){

    }

    SEXP DPH(){

    }

};

RCPP_MODULE(ScoringFeatures) {
    using namespace Rcpp;

    class_<ScoringFeatures > ("ScoringFeatures")
            .constructor<arma::mat, arma::mat, vector<string> >("Default Constructor")
            .method("getSimilarityMatrix", &ScoringFeatures::getSimilarityMatrix, "Test Documentation");

            // read-only property;
}
