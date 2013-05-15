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
    struct Gram {
		//std::vector<std::string> terms;
		std::string term;
		int internal_termID;

		struct hash {
			int operator() ( const Gram* one ) const {
				indri::utility::GenericHash<const char*> h;
				int accumulator = 0;

				//for( size_t i=0; i<one->terms.size(); i++ ) {
					accumulator *= 7;
					accumulator += h( one->term.c_str() );
					//}

					return accumulator;
			}
		};


		struct weight_greater {
			bool operator() ( const Gram* o, const Gram* t ) const {
				return t->internal_termID > o->internal_termID;
			}
		};

		struct string_comparator {
			int operator() ( const Gram* o, const Gram* t ) const {
				const Gram& one = *o;
				const Gram& two = *t;

				/*if( one.terms.size() != two.terms.size() ) {
    	                    if( one.terms.size() < two.terms.size() ) {
    	                      return 1;
    	                    } else {
    	                      return -1;
    	                    }
    	                  }*/

				//for( size_t i=0; i<one.terms.size(); i++ ) {
				const std::string& oneString = one.term;//s[i];
				const std::string& twoString = two.term;//s[i];

				if( oneString != twoString ) {
					if( oneString < twoString )
						return -1;
					else
						return 1;
				}
				//}

				return 0;
			}
		};
	};

    struct Results{
        map<string, int> queryStems;
        map<string, int> queryStemIndex;
        vector<string> queryStemOrder;
        arma::mat tfMatrix;
        arma::vec dfVector;
        arma::vec ctfVector;
    };

    struct GramCounts {
         Gram gram;
         indri::utility::greedy_vector< std::pair< int, int > > counts;
     };

    typedef indri::utility::HashTable< Gram*, GramCounts*, Gram::hash, Gram::string_comparator > HGram;

private:

    HGram _gramTable;
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

    SEXP close_index() {
        environment.close();
        return (Rcpp::wrap(true));
    }

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

    SEXP getQueryStats(){
        vector<int> qTF;
        vector<int> qIndex;
        for(int i=0; i < resultsData.queryStemOrder.size(); i++ ){
            string term = resultsData.queryStemOrder.at(i);
            qTF.push_back(resultsData.queryStems[term]);
            qIndex.push_back(resultsData.queryStemIndex[term]);
        }
        DataFrame d = DataFrame::create( Named("qTF")= qTF,
                                Named("qIndex") = qIndex);
        d.attr("row.names") = resultsData.queryStemOrder;
        return d;

    }

    SEXP getDocTermMatrix(string termWeighting){
        Rcpp::List dimnms = Rcpp::List::create(extDocIDs, terms);
        if(termWeighting == "tf"){
            NumericMatrix d = Rcpp::wrap(resultsData.tfMatrix);
            d.attr("dimnames") = dimnms;
            return d;
        }
        else if(termWeighting == "tfidf"){
            arma::mat tfidfMat = resultsData.tfMatrix;
            arma::vec idf = arma::log((environment.documentCount() + 1) /
                    (resultsData.dfVector + 0.5));
            tfidfMat.each_row() %= idf.t();
            NumericMatrix d = Rcpp::wrap(tfidfMat);
            d.attr("dimnames") = dimnms;
            return d;
        }else if(termWeighting == "idf"){

        }


    }

    SEXP getTermStats(){

        vector<string> statName;
        statName.push_back("DocFreq");
        statName.push_back("IDF");
        statName.push_back("cTF");
        arma::vec idf = arma::log((environment.documentCount() + 1) /
                (resultsData.dfVector + 0.5));
        DataFrame d = DataFrame::create(Named("DocFreq")=resultsData.dfVector,
                                        Named("IDF")=idf,
                                        Named("cTF")=resultsData.ctfVector);
        d.attr("row.names") = terms;
        return d;
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

    SEXP setScoringRules(string method){
        environment.setBaseline();
        return R_NilValue;
    }

    SEXP generateResultsFromSet(string _qno, string _query, vector<string> docSet){
        resultsData = resultsData_nullCopy;
        documentIDs.clear();
        scores.clear();
        extDocIDs.clear();
        terms.clear();
        _gramTable.clear();
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

    SEXP generateResults(string _qno, string _query, int _documentLimit) {

        resultsData = resultsData_nullCopy;
        documentIDs.clear();
        scores.clear();
        extDocIDs.clear();
        terms.clear();
        _gramTable.clear();
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

        updateQueryDetails(environment, resultsData, query);
        countGrams();
        buildStats();

        return Rcpp::wrap(true);
    }

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
        // for each query result
        for( size_t i=0; i< results.size(); i++ ) {
            // run through the text, extracting n-grams
            indri::api::ScoredExtentResult& result = results[i];
            indri::api::DocumentVector* v = vectors[i];
            std::vector<int>& positions = v->positions();
            std::vector<std::string>& stems = v->stems();
            std::vector< indri::api::DocumentVector::Field >& fields = v->fields();
            if (result.end == 0) result.end = positions.size();
            // for each word position in the text
            for( int j = result.begin; j < result.end; j++ ) {
                //int maxGram = std::min( _maxGrams, result.end - j );

                GramCounts* newCounts = new GramCounts;
                bool containsOOV = false;

                // build the gram

                if( positions[ j ] == 0 || (! isValid(stems[ positions[ j ] ])) ) {
                    containsOOV = true;
                    continue;
                }

                newCounts->gram.term =  stems[ positions[ j ] ] ;
                if( containsOOV ) {
                    // if this contanied OOV, all larger n-grams
                    // starting at this point also will
                    delete newCounts;
                    break;
                }

                GramCounts** gramCounts = 0;
                gramCounts = _gramTable.find( &newCounts->gram );
                if( gramCounts == 0 ) {
                    _gramTable.insert( &newCounts->gram, newCounts );
                    gramCounts = &newCounts;
                } else {
                    delete newCounts;
                }
                if( (*gramCounts)->counts.size() && (*gramCounts)->counts.back().first == i ) {
                    // we already have some counts going for this query result, so just add this one
                    (*gramCounts)->counts.back().second++;
                } else {
                    // no counts yet in this document, so add an entry
                    (*gramCounts)->counts.push_back( std::make_pair( i, 1 ) );
                }
            }
        }
        for (unsigned int i = 0; i < vectors.size(); i++)
            delete vectors[i];
    }

    void buildStats() {
        HGram::iterator iter;
        resultsData.tfMatrix = arma::zeros<arma::mat>(results.size(),
                                                      _gramTable.size());
        // Initialize the
        resultsData.dfVector.set_size(_gramTable.size());
        resultsData.ctfVector.set_size(_gramTable.size());

        int tmpTermID = -1;
        for( iter = _gramTable.begin(); iter != _gramTable.end(); iter++ ) {
            double gramCount = 0;
            ++tmpTermID;
            Gram* gram = *iter->first;
            GramCounts* gramCounts = *iter->second;
            gram->internal_termID = tmpTermID;
            terms.push_back(gram->term);
             if( resultsData.queryStems.find(gram->term) != resultsData.queryStems.end() )
                resultsData.queryStemIndex[gram->term] = tmpTermID;

            resultsData.ctfVector(tmpTermID) = environment.stemCount(gram->term);
            resultsData.dfVector(tmpTermID) =  environment.documentStemCount(gram->term);
            size_t c, r;
            for( r = 0, c = 0; r < results.size() && c < gramCounts->counts.size(); r++ ) {
                if( gramCounts->counts[c].first == r ) {
                    resultsData.tfMatrix(r, tmpTermID) = gramCounts->counts[c].second;
                    c++;
                }
            }
        }
        _gramTable.clear();
    }
};

RCPP_MODULE(Index) {
    using namespace Rcpp;

    class_<Index > ("Index")
            .constructor<string, bool>("Default Constructor")
            // read-only property
            .method("stemTerm", &Index::stemTerm, "Test Documentation")
            .method("docFreq", &Index::docFreq, "Test Documentation")
            .method("collFreq", &Index::collFreq, "Test Documentation")
            .method("getTermCount", &Index::getTermCount, "Test Documentation")
            .method("getDocCount", &Index::getDocCount, "Test Documentation")
            .method("generateResults", &Index::generateResults, "Test Documentation")
            .method("generateResultsFromSet", &Index::generateResultsFromSet, "Test Documentation")
            .method("getQueryStats", &Index::getQueryStats, "Test Documentation")
            .method("getRanking", &Index::getRanking, "Test Documentation")
            .method("getDocTermMatrix", &Index::getDocTermMatrix, "Test Documentation")
            .method("getTermStats", &Index::getTermStats, "Test Documentation")
            .method("generateSnippets", &Index::generateSnippets, "Test Documentation")
            .method("getMetaData", &Index::getMetaData, "Test Documentation");
}
