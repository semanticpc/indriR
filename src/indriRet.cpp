#include "utils.h"
#include "indri/QueryParserFactory.hpp"
#include "indri/QueryEnvironment.hpp"
#include "indri/QueryParserFactory.hpp"
#include "indri/RawScorerNodeExtractor.hpp"
#include "indri/SnippetBuilder.hpp"
using namespace Rcpp;
using namespace std;

class Index {
public:
    struct Results{
        map<string, int> queryStems;
        map<string, int> queryStemIndex;
        map<string, int> fieldIndex;
        vector<string> queryStemOrder;
        arma::cube tfMatrix;
        arma::mat dfVector;
        arma::mat ctfVector;

    };


    indri::utility::HashTable< string, int> termIDs;

private:

    std::vector<string> terms;
    indri::api::QueryEnvironment environment;

    indri::api::QueryAnnotation* qa;
    std::vector<lemur::api::DOCID_T> documentIDs;
    std::vector<indri::api::ScoredExtentResult> results;
    std::vector<double> scores;
    std::vector<string> extDocIDs;
    string qno;
    string query;
    int documentLimit;
    Results resultsData;

    Results resultsData_nullCopy;



public:

    Index(string _indexPath, bool _server) {
        try {
            if (_server) environment.addServer(_indexPath);
            else environment.addIndex(_indexPath);
        } catch (std::exception &ex) {
            forward_exception_to_r(ex);
        } catch (lemur::api::Exception& e) {
            ::Rf_error("Unable to open index");
        } catch (...) {
            ::Rf_error("Caught unhandled exception");
        }
    }


    // Index Environment Functions

    SEXP addIndex(string _indexPath){
        try {
            environment.addIndex(_indexPath);
        } catch (std::exception &ex) {
            forward_exception_to_r(ex);
        } catch (lemur::api::Exception& e) {
            ::Rf_error("Unable to open index");
        } catch (...) {
            ::Rf_error("Caught unhandled exception");
        }
    }

    SEXP addServer(string _server){
        try {
            environment.addServer(_server);
        } catch (std::exception &ex) {
            forward_exception_to_r(ex);
        } catch (lemur::api::Exception& e) {
            ::Rf_error("Unable to open index");
        } catch (...) {
            ::Rf_error("Caught unhandled exception");
        }
    }

    SEXP closeIndex() {
        environment.close();
        return (Rcpp::wrap(true));
    }

    SEXP setScoringRules(string method, string parameters){
        vector<string> scoringRules;
        //if(method == "tfidf" || method == "Okapi" || method == "BM25" ){
          //  string rule = method + "," + parameters;
            //environment.setBaseline(rule);
            //scoringRules.push_back("");
            //environment.setScoringRules(scoringRules);
        //}else{
            string rule = "method:" + method + "," + parameters;
            scoringRules.push_back(rule);
            environment.setScoringRules(scoringRules);
        //}


        return R_NilValue;
    }

    SEXP setMemory (int memory){
        return R_NilValue;
    }


    // Run Query Functions

    SEXP getRanking(string _runid="default"){
            vector<string> res_qno;
            vector<string> res_q0;
            vector<string> res_runid;

            for(int i=0; i < documentLimit; i++){
                res_qno.push_back(qno);
                res_q0.push_back("Q0");
                res_runid.push_back(_runid);

            }

            return Rcpp::DataFrame::create( Named("topic")= qno,
                    Named("q0")= res_q0, Named("docID")=  wrap(extDocIDs),
                    Named("rank")= seq( 1, documentLimit ),
                    Named("score")= wrap(scores),
                    Named("runID")= res_runid);
        }


    SEXP runIndriModel(){
        indri::api::QueryAnnotation* qa;
        qa = environment.runAnnotatedQuery(query, documentIDs, documentLimit);

        std::vector<indri::api::ScoredExtentResult> results = qa->getResults();
        //_logtoposterior(results);

        // Extract Documents
        std::vector<lemur::api::DOCID_T> documentIDs;
        std::vector<double> scores;
        for (size_t i = 0; i < results.size(); i++){
            documentIDs.push_back(results[i].document);
            scores.push_back(results[i].score);
        }

        std::vector<string> extDocIDs = environment.documentMetadata(documentIDs, "docno");
        Rcpp::NumericVector result = wrap(scores);
        result.attr("names") = extDocIDs;
        return result;
    }

    SEXP runQuery(string _qno, string _query, string _runid="default"){
        indri::api::QueryAnnotation* qa;
        qa = environment.runAnnotatedQuery(_query, documentIDs, documentLimit);

        std::vector<indri::api::ScoredExtentResult> results = qa->getResults();
        //_logtoposterior(results);

        // Extract Documents
        std::vector<lemur::api::DOCID_T> documentIDs;
        std::vector<double> scores;
        for (size_t i = 0; i < results.size(); i++){
            documentIDs.push_back(results[i].document);
            scores.push_back(results[i].score);
        }
        vector<string> res_qno;
        vector<string> res_q0;
        vector<string> res_runid;
//
//        int documentLimit = _documentLimit;

        for(int i=0; i < documentLimit; i++){
            res_qno.push_back(qno);
            res_q0.push_back("Q0");
            res_runid.push_back(_runid);

        }
        std::vector<string> extDocIDs = environment.documentMetadata(documentIDs, "docno");
        return Rcpp::DataFrame::create( Named("topic")= _qno,
                Named("q0")= res_q0, Named("docID")=  wrap(extDocIDs),
                Named("rank")= seq( 1, documentLimit ),
                Named("score")= wrap(scores),
                Named("runID")= res_runid);
        }

    SEXP generateResultsFromSet(string _qno, string _query, vector<string> docSet){
        resultsData = resultsData_nullCopy;
        documentIDs.clear();
        scores.clear();
        extDocIDs.clear();
        terms.clear();
        termIDs.clear();
        results.clear();
        qno = _qno;
        query = _query;

        documentLimit = docSet.size();
        documentIDs = environment.documentIDsFromMetadata("docno", docSet);
        qa = environment.runAnnotatedQuery(query, documentIDs, documentLimit);


        results = qa->getResults();
        _logtoposterior(results);

        // Extract Documents
        for (size_t i = 0; i < results.size(); i++){
            scores.push_back(results[i].score);
        }
        extDocIDs = environment.documentMetadata(documentIDs, "docno");

        updateQueryDetails(environment, resultsData, query);
        countGrams();
        buildStats();

        return Rcpp::wrap(true);
    }

    SEXP generateResults(string _qno, string _query, int _documentLimit, bool stats) {

        resultsData = resultsData_nullCopy;
        documentIDs.clear();
        scores.clear();
        extDocIDs.clear();
        terms.clear();
        termIDs.clear();
        results.clear();
        qno = _qno;
        query = _query;



        documentLimit = _documentLimit;
        qa = environment.runAnnotatedQuery(query, _documentLimit);


        results = qa->getResults();
        _logtoposterior(results);

        // Extract Documents
        for (size_t i = 0; i < results.size(); i++){
            documentIDs.push_back(results[i].document);
            scores.push_back(results[i].score);
        }
        extDocIDs = environment.documentMetadata(documentIDs, "docno");
        if(stats){
            updateQueryDetails(environment, resultsData, query);
            countGrams();
            buildStats();
        }

        return Rcpp::wrap(true);
    }


    // Index Statistics

    SEXP getDocCount() {
        long res = environment.documentCount();
        return Rcpp::wrap(res);
    }

    SEXP getTermCount() {
        long res = environment.termCount();
        return Rcpp::wrap(res);
    }

    SEXP stemTerm(string _term) {
        return Rcpp::wrap(environment.stemTerm(_term));
    }

    SEXP docFreq(string _term) {
        long res = environment.documentCount(_term);
        return Rcpp::wrap(res);
    }

    SEXP collFreq(string _term) {
        long res = environment.termCount(_term);
        return Rcpp::wrap(res);
    }



    // Term and Query Statistics

    SEXP getQueryStats(){
        vector<int> qTF;
        vector<int> qIndex;
        for(int i=0; i < resultsData.queryStemOrder.size(); i++ ){
            string term = resultsData.queryStemOrder.at(i);
            qTF.push_back(resultsData.queryStems[term]);
            qIndex.push_back(resultsData.queryStemIndex[term] + 1);
        }
        DataFrame d = DataFrame::create( Named("qTF")= qTF,
                                Named("qIndex") = qIndex);
        d.attr("row.names") = resultsData.queryStemOrder;
        return d;

    }

    SEXP getTermStats(string fname){

        int findex = resultsData.fieldIndex[fname];
        arma::vec idf = arma::log((environment.documentCount() + 1) /
                (resultsData.dfVector.col(findex) + 0.5));
        arma::vec df = resultsData.dfVector.col(findex);
        arma::vec ctf = resultsData.ctfVector.col(findex);
        DataFrame d = DataFrame::create(Named("DocFreq")=df,
                                        Named("IDF")=idf,
                                        Named("cTF")=ctf);
        d.attr("row.names") = terms;
        return d;
    }



    // Other Functions

    SEXP getTFMatrix(string fname){
        int findex = resultsData.fieldIndex[fname];
        Rcpp::List dimnms = Rcpp::List::create(extDocIDs, terms);

        NumericMatrix d = Rcpp::wrap(resultsData.tfMatrix.slice(findex));
        d.attr("dimnames") = dimnms;
        return d;
//        }else if(termWeighting == "tf_normalized"){
//            arma::mat tfnorm = resultsData.tfMatrix.slice(findex);
//            arma::rowvec docLen = arma::sum(tfnorm, 0);
//            tfnorm.each_row() /= docLen;
//            NumericMatrix d = Rcpp::wrap(tfnorm);
//            d.attr("dimnames") = dimnms;
//            return d;
//        }else if(termWeighting == "tfidf"){
//            arma::mat tfidfMat = resultsData.tfMatrix.slice(findex);
//            arma::vec idf = arma::log((environment.documentCount() + 1) /
//                    (resultsData.dfVector + 0.5));
//            tfidfMat.each_row() %= idf.t();
//
//            NumericMatrix d = Rcpp::wrap(tfidfMat);
//            d.attr("dimnames") = dimnms;
//            return d;
//        }else if(termWeighting == "idf"){
//
//        }


    }

    SEXP generateSnippets(bool html){
        vector<string> snippetString;
        vector< indri::api::ParsedDocument* > pdocuments = environment.documents(documentIDs);
        indri::api::SnippetBuilder sp(html);

        for( size_t row=0; row < documentIDs.size(); row++ )
            snippetString.push_back(sp.build(documentIDs[row], pdocuments[row], qa));
        CharacterVector c = wrap(snippetString);
        c.attr("names") = extDocIDs;
        return c;
    }

    SEXP getMetaData(string metaDataKey){
        vector<string> metaDataString = environment.documentMetadata(documentIDs, metaDataKey);
        CharacterVector c = wrap(metaDataString);
        c.attr("names") = extDocIDs;
        return c;
    }

    SEXP getDocumentLengths(){
        vector<int> docLength;
        for (size_t i = 0; i < documentIDs.size(); i++)
            docLength.push_back(environment.documentLength(documentIDs[i]));
        return wrap(docLength);
    }



    // Helper Functions

    void _logtoposterior(std::vector<indri::api::ScoredExtentResult> &res){
        if (res.size() == 0) return;
        std::vector<indri::api::ScoredExtentResult>::iterator iter;
        iter = res.begin();
        double K = (*iter).score;
        // first is max
        double sum = 0;

        for (iter = res.begin(); iter != res.end(); iter++) {
            sum += (*iter).score = exp(K + (*iter).score);
        }
        for (iter = res.begin(); iter != res.end(); iter++) {
            (*iter).score /= sum;
        }
    }

    void updateQueryDetails(indri::api::QueryEnvironment& environment,
                            Results& resultData,
                            string query){

        indri::api::QueryParserWrapper *parser = indri::api::QueryParserFactory::get(query, "indri");
        indri::lang::ScoredExtentNode* rootNode = parser->query();
        indri::lang::RawScorerNodeExtractor extractor;
        rootNode->walk(extractor);
        vector<indri::lang::RawScorerNode*>& scorerNodes = extractor.getScorerNodes();

        for (int i = 0; i < scorerNodes.size(); i++){
            string qterm = environment.stemTerm(scorerNodes[i]->queryText());
            if(environment.stemCount(qterm) == 0)
                continue;
            if( resultData.queryStems.find(qterm) == resultData.queryStems.end() ){
                resultData.queryStems.insert(make_pair( qterm, 1));
                resultData.queryStemOrder.push_back(qterm);
            }
            else
                resultData.queryStems[qterm] += 1;
        }
    }

    bool isValid(const string & word){
        size_t length = word.size();
        const char * chArray = word.c_str();
        size_t pos = 0;

        while (pos < length){
            if(isalnum((unsigned char)*(chArray+pos)) == 0){
                return false;
            }
            pos ++;
        }
        return true;
    }

    void countGrams() {
        std::vector<indri::api::DocumentVector*> vectors =
                environment.documentVectors( documentIDs );

        int termCount = -1;

        for( size_t i=0; i< results.size(); i++ ) {
            indri::api::ScoredExtentResult& result = results[i];
            indri::api::DocumentVector* v = vectors[i];
            std::vector<int>& positions = v->positions();
            std::vector<std::string>& stems = v->stems();
            std::vector< indri::api::DocumentVector::Field >& fields = v->fields();



            if (result.end == 0) result.end = positions.size();
            for( int j = result.begin; j < result.end; j++ ) {
                bool containsOOV = false;
                if( positions[ j ] == 0 || (! isValid(stems[ positions[ j ] ])) ) {
                    containsOOV = true;
                    continue;
                }
                string term =  stems[ positions[ j ] ] ;
                if( termIDs.find(term) == 0 )
                    termIDs.insert(term, ++termCount);
            }
        }

        vector<string> flVec = environment.fieldList();
        resultsData.fieldIndex["all"] = 0;
        for(int fl=0; fl <  flVec.size(); fl++)
            resultsData.fieldIndex[flVec[fl]] = fl + 1;

        resultsData.tfMatrix = arma::zeros(results.size(), termIDs.size(), flVec.size() + 1);
        for( size_t i=0; i< results.size(); i++ ) {
            indri::api::ScoredExtentResult& result = results[i];
            indri::api::DocumentVector* v = vectors[i];
            std::vector<int>& positions = v->positions();
            std::vector<std::string>& stems = v->stems();
            std::vector< indri::api::DocumentVector::Field >& fields = v->fields();
            if (result.end == 0) result.end = positions.size();
            for( int j = result.begin; j < result.end; j++ ) {
                bool containsOOV = false;

                if( positions[ j ] == 0 || (! isValid(stems[ positions[ j ] ])) ) {
                    containsOOV = true;
                    continue;
                }
                string term =  stems[ positions[ j ] ] ;

                int id = (*termIDs.find(term));
                resultsData.tfMatrix(i, id, 0) += 1;
            }
            for( int f = 0; f < fields.size(); f++ ) {
                map<string, int>::iterator iter = resultsData.fieldIndex.find(fields[f].name);
                int f_index = -1;
                if(iter != resultsData.fieldIndex.end())
                    f_index = resultsData.fieldIndex[fields[f].name];
                else{
                    cout << fields[f].name << endl;
                    continue;
                }

                for( int j = fields[f].begin; j < fields[f].end; j++ ) {
                    bool containsOOV = false;

                    if( positions[ j ] == 0 || (! isValid(stems[ positions[ j ] ])) ) {
                        containsOOV = true;
                        continue;
                    }
                    string term =  stems[ positions[ j ] ] ;

                    int id = (*termIDs.find(term));
                    resultsData.tfMatrix(i, id, f_index) += 1;
                }
            }
        }
        for (unsigned int i = 0; i < vectors.size(); i++)
            delete vectors[i];
    }

    void buildStats() {
            resultsData.ctfVector = arma::zeros(termIDs.size(), resultsData.fieldIndex.size());
            resultsData.dfVector = arma::zeros(termIDs.size(), resultsData.fieldIndex.size());
            terms = std::vector<string>(termIDs.size(), "");
            indri::utility::HashTable<string, int>::iterator iter;
            for( iter = termIDs.begin(); iter != termIDs.end(); iter++ ) {
                string term = *iter->first;
                int id = *iter->second;
                terms.at(id) = term;
                 if( resultsData.queryStems.find(term) != resultsData.queryStems.end() )
                    resultsData.queryStemIndex[term] = id;

                resultsData.ctfVector(id, 0) = environment.stemCount(term);
                resultsData.dfVector(id, 0) =  environment.documentStemCount(term);
                map<string, int>::iterator fl_iter;
                for(fl_iter = resultsData.fieldIndex.begin() ;
                        fl_iter != resultsData.fieldIndex.end(); fl_iter++ ) {
                    string fl_name = fl_iter->first;
                    int fl_index = fl_iter->second;
                    resultsData.ctfVector(id, fl_index) = environment.stemFieldCount(term, fl_name);
                }

            }

            map<string, int>::iterator fl_iter;
            for(fl_iter = resultsData.fieldIndex.begin() ;
                    fl_iter != resultsData.fieldIndex.end(); fl_iter++ ) {
                    string fl_name = fl_iter->first;
                    int fl_index = fl_iter->second;
                    arma::mat tf_mat = resultsData.tfMatrix.slice(fl_index);
                    arma::umat df_mat =   tf_mat > arma::zeros(tf_mat.n_rows,tf_mat.n_cols);
                    resultsData.dfVector.col(fl_index) = arma::conv_to<arma::mat>::from(sum(df_mat, 0)).t();
            }


    }
};

RCPP_MODULE(Index) {
    using namespace Rcpp;

    class_<Index > ("Index")

    .constructor<string, bool>("Default Constructor")

    // Index Environment Functions
    .method("addIndex", &Index::addIndex, "Test Documentation")
    .method("addServer", &Index::addServer, "Test Documentation")
    .method("setScoringRules", &Index::setScoringRules, "Test Documentation")
    .method("setMemory", &Index::setMemory, "Test Documentation")


    .method("stemTerm", &Index::stemTerm, "Test Documentation")

    // Index Statistic

    .method("docFreq", &Index::docFreq, "Test Documentation")
    .method("collFreq", &Index::collFreq, "Test Documentation")
    .method("getTermCount", &Index::getTermCount, "Test Documentation")
    .method("getDocCount", &Index::getDocCount, "Test Documentation")


    // Run Query Methods
    .method("generateResults", &Index::generateResults, "Test Documentation")
    .method("generateResultsFromSet", &Index::generateResultsFromSet, "Test Documentation")
    .method("getRanking", &Index::getRanking, "Test Documentation")
    .method("runQuery", &Index::runQuery, "Test Documentation")
    .method("runIndriModel", &Index::runIndriModel, "Test Documentation")


    // Term and Query Statistics
    .method("getTermStats", &Index::getTermStats, "Test Documentation")
    .method("getQueryStats", &Index::getQueryStats, "Test Documentation")


    // Other Features
    .method("getTFMatrix", &Index::getTFMatrix, "Test Documentation")
    .method("getDocumentLengths", &Index::getDocumentLengths, "Test Documentation")
    .method("generateSnippets", &Index::generateSnippets, "Test Documentation")
    .method("getMetaData", &Index::getMetaData, "Test Documentation");
}
