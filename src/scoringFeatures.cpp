#include "utils.h"
using namespace Rcpp;
using namespace std;

class ScoringFeatures {
public:

private:
    arma::mat termDocMat;
    arma::mat q_termDocMat;
    arma::rowvec q_idf;
    arma::rowvec q_ctf;
    arma::rowvec q_df;
    vector<string> extDocumentIDs;
public:

    ScoringFeatures(arma::mat _termDocMatrix,
                    arma::mat _queryTermStats,
                    arma::uvec qTermIndices,
                    vector<string> _docIDs) {
        termDocMat = _termDocMatrix;
        qTermIndices -= 1;
        q_termDocMat = _termDocMatrix.cols(qTermIndices);
        q_df = _queryTermStats.col(0).t();
        q_idf = _queryTermStats.col(1).t();
        q_ctf = _queryTermStats.col(2).t();
        extDocumentIDs = _docIDs;

    }

    SEXP getDocumentIDs(){
        return wrap(extDocumentIDs);
    }
    SEXP getSimilarityMatrix(){
        arma::mat tfidfMat = termDocMat;
        tfidfMat.each_row() %= q_idf;
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

    SEXP numOfQTerms(){
        arma::mat numOfQTerms = arma::conv_to<arma::mat>::from(q_termDocMat >
                                arma::zeros(q_termDocMat.n_rows, q_termDocMat.n_cols));
        NumericVector result = wrap(arma::sum(numOfQTerms, 1));
        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;

    }

    SEXP ratioOfQTerms(){
        arma::mat numOfQTerms = arma::conv_to<arma::mat>::from(q_termDocMat >
                                arma::zeros(q_termDocMat.n_rows, q_termDocMat.n_cols));
        NumericVector result = wrap(arma::sum(numOfQTerms, 1) / q_termDocMat.n_cols);
        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;
    }

    SEXP termFrequency(string stat_type){
        NumericVector result;
        if ( stat_type == "sum")
            result = wrap(arma::sum(q_termDocMat, 1));
        else if ( stat_type == "min")
            result = wrap(arma::min(q_termDocMat, 1));
        else if ( stat_type == "max")
            result = wrap(arma::max(q_termDocMat, 1));
        else if ( stat_type == "var")
            result = wrap(arma::var(q_termDocMat, 0, 1));
        else if ( stat_type == "mean")
            result = wrap(arma::mean(q_termDocMat, 1));

        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;


    }

    SEXP getStreamLen(){
        NumericVector result = wrap(arma::sum(termDocMat, 1));
        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;
    }

    SEXP lenNormTermFrequency(string stat_type){
        arma::mat normTFMat = q_termDocMat;

        normTFMat.each_col() /= (as<arma::vec>(getStreamLen()) + 1);

        NumericVector result;
        if ( stat_type == "sum")
            result =  wrap(arma::sum(normTFMat, 1));
        else if ( stat_type == "min")
            result =  wrap(arma::min(normTFMat, 1));
        else if ( stat_type == "max")
            result =  wrap(arma::max(normTFMat, 1));
        else if ( stat_type == "var")
            result =  wrap(arma::var(normTFMat, 0, 1));
        else if ( stat_type == "mean")
            result =  wrap(arma::mean(normTFMat, 1));

        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;

    }

    SEXP TFIDF(string stat_type){
        arma::mat TFIDFMat = q_termDocMat;
        TFIDFMat.each_row() %= q_idf;
        NumericVector result;
        if ( stat_type == "sum")
            result = wrap(arma::sum(TFIDFMat, 1));
        else if ( stat_type == "min")
            result = wrap(arma::min(TFIDFMat, 1));
        else if ( stat_type == "max")
            result = wrap(arma::max(TFIDFMat, 1));
        else if ( stat_type == "var")
            result = wrap(arma::var(TFIDFMat, 0, 1));
        else if ( stat_type == "mean")
            result = wrap(arma::mean(TFIDFMat, 1));

        result.attr("dimnames") = List::create(extDocumentIDs);
        return result;

    }





    SEXP BM25(){

    }

    SEXP LMD(){

    }

    SEXP LMJM(){

    }

};

RCPP_MODULE(ScoringFeatures) {
    using namespace Rcpp;

    class_<ScoringFeatures > ("ScoringFeatures")
            .constructor<arma::mat, arma::mat, arma::uvec, vector<string> >("Default Constructor")
            .method("getSimilarityMatrix", &ScoringFeatures::getSimilarityMatrix, "Test Documentation")
            .method("numOfQTerms", &ScoringFeatures::numOfQTerms, "Test Documentation")
            .method("ratioOfQTerms", &ScoringFeatures::ratioOfQTerms, "Test Documentation")
            .method("getStreamLen", &ScoringFeatures::getStreamLen, "Test Documentation")
            .method("termFrequency", &ScoringFeatures::termFrequency, "Test Documentation")
            .method("lenNormTermFrequency", &ScoringFeatures::lenNormTermFrequency, "Test Documentation")
            .method("TFIDF", &ScoringFeatures::TFIDF, "Test Documentation")
            .method("getDocumentIDs", &ScoringFeatures::getDocumentIDs, "Test Documentation");

            // read-only property;
}
