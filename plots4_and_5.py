import numpy as np
import pandas as pd
from collections import defaultdict

# ai classifications, WIPO
ai_filter1_all_matches = np.load('data/ai_filter1_all_matches_nosummary.npy').tolist()
ai_filter2_all_matches = np.load('data/ai_filter2_all_matches_nosummary.npy').tolist()
ai_filter3_all_matches = np.load('data/ai_filter3_all_matches_nosummary.npy').tolist()
ai_patnums = np.union1d(ai_filter1_all_matches,ai_filter2_all_matches)
ai_patnums = np.union1d(ai_filter3_all_matches,ai_patnums)
print("Number of AI patnums", len(ai_patnums))

citations_with_all_targets_recent = pd.read_csv('data/citations_with_all_targets_recent.csv')
citations_with_climate_targets_recent = pd.read_csv('data/citations_with_climate_targets_recent.csv')
df_CPC_latest = pd.read_csv('data/df_CPC_latest.csv')

def counts_and_null_model(citations_raw):
    # distinguish between source patents (making the citations) and target patents (getting the forward citations)
    citations = citations_raw.copy(deep=True)
    citations.rename({'grantyear':'target_grantyear'},axis=1,inplace=True)
    citations.rename({'patnum':'source'},axis=1,inplace=True)
    citations['source_ai'] = citations.source.isin(ai_patnums).astype(int)
    citations['source_other'] = (~citations['source_ai'].astype(bool)) & (~citations['source_green'].astype(bool)).astype(int)
    citations['target_other'] = (~citations['target_ai'].astype(bool)) & (~citations['target_green'].astype(bool)).astype(int)
    
    classes = ['ai','green','other']
    t = citations.source_grantyear.max() # should be 2019
    print("t =",t)
    C = dict()
    for lag in range(2010,t+1):
        for i in classes:
            target_classes = classes
            for j in target_classes:
                source_var = 'source_'+i
                target_var = 'target_'+j
                # only for patents granted in year t
                lag_index = t-lag
                query = "source_grantyear==" + str(t) + " and target_grantyear==" + str(lag) + " and " + source_var+"==1 and "+target_var+"==1"
                n_citations = len(citations.query(query))
                key = tuple([i,j,t,lag_index])
                C[key] = n_citations

    # observed counts in total (at time t): 
    # notation of i, j is i:source of citations, j:target for citations
    Ctotal = dict()
    for i in classes:
        target_classes = classes
        for j in target_classes:
            total = 0
            for lag in range(2010,t+1):
                lag_index = t-lag
                key = tuple([i,j,t,lag_index])
                total += C[key]
    
            key = tuple([i,j,t])
            Ctotal[key] = total

    S = dict()
    for lag in range(2010,t+1):
        lag_index = t-lag
        inter_domain_citations = 0
        for i in classes:
            target_classes = classes
            for j in target_classes:
                key = tuple([i,j,t,lag_index])
                inter_domain_citations += C[key]                

        key = tuple([t,lag_index])       
        S[key] = inter_domain_citations
    
    N = dict() # number of citations sent by class j to patents at time t-lag
    for lag in range(2010,t+1):
        lag_index = t-lag        
        for i in classes:
            n_citations_from_class = 0
            target_classes = classes
            for j in target_classes:
                key = tuple([i,j,t,lag_index])
                n_citations_from_class += C[key]

            key = tuple([i,lag_index])            
            N[key] = n_citations_from_class
                        
    K = dict() # number of citations received by class j at time t-lag
    for lag in range(2010,t+1):
        lag_index = t-lag        
        for j in classes:
            n_citations_to_class = 0
            target_classes = classes
            for i in target_classes:
                key = tuple([i,j,t,lag_index])
                n_citations_to_class += C[key]
    
            key = tuple([j,lag_index])            
            K[key] = n_citations_to_class

    ECijt = dict()
    for i in classes:
        target_classes = classes
        for j in target_classes:
            expected = 0
            for lag in range(2010,t+1):
                lag_index = t-lag
                key_tlag = tuple([t,lag_index])
                key_ilag = tuple([i,lag_index])
                key_jlag = tuple([j,lag_index])
                k = K[key_jlag]
                s = S[key_tlag]
                n = N[key_ilag]
                
                expected += k*n/s
            key = tuple([i,j,t])
            ECijt[key] = expected

    StdDevs = dict()
    for i in classes:
        target_classes = classes
        for j in target_classes:
            squared = 0
            for lag in range(2010,t+1):
                lag_index = t-lag
                key_tlag = tuple([t,lag_index])
                key_ilag = tuple([i,lag_index])
                key_jlag = tuple([j,lag_index])
                k = K[key_jlag]
                s = S[key_tlag]
                n = N[key_ilag]
                
                # k: number of successes/number of citations received by j
                # s: total number of inter-domain citations
                # n: number of draws/citations made by a given class i
                term = n*(k/s)*(1-k/s)*((s-n)/(s-1))                
                squared += term

            stddevs = np.sqrt(squared)
            key = tuple([i,j,t])
            StdDevs[key] = stddevs
            
    ZScores = dict()
    for i in classes:
        target_classes = classes
        for j in target_classes:
            key = tuple([i,j,t])
            expected = ECijt[key]
            observed = Ctotal[key]
            stddev = StdDevs[key]

            zscore = (observed-expected)/stddev
            ZScores[key] = zscore

    Ctotalagg = dict()
    Ctotalagg[tuple(['non-climate','non-AI',2019])] = Ctotal[tuple(['ai','other',2019])] + Ctotal[tuple(['ai','green',2019])] +\
                                                      Ctotal[tuple(['other','other',2019])] + Ctotal[tuple(['other','green',2019])]
    Ctotalagg[tuple(['non-climate','AI',2019])] = Ctotal[tuple(['ai','ai',2019])] + Ctotal[tuple(['other','ai',2019])]
    Ctotalagg[tuple(['climate','AI',2019])] = Ctotal[tuple(['green','ai',2019])]
    Ctotalagg[tuple(['climate','non-AI',2019])] = Ctotal[tuple(['green','other',2019])] + Ctotal[tuple(['green','green',2019])]

    ECijtagg = dict()
    ECijtagg[tuple(['non-climate','non-AI',2019])] = ECijt[tuple(['ai','other',2019])] + ECijt[tuple(['ai','green',2019])] +\
                                                      ECijt[tuple(['other','other',2019])] + ECijt[tuple(['other','green',2019])]
    ECijtagg[tuple(['non-climate','AI',2019])] = ECijt[tuple(['ai','ai',2019])] + ECijt[tuple(['other','ai',2019])]
    ECijtagg[tuple(['climate','AI',2019])] = ECijt[tuple(['green','ai',2019])]
    ECijtagg[tuple(['climate','non-AI',2019])] = ECijt[tuple(['green','other',2019])] + ECijt[tuple(['green','green',2019])]

    # Color the different parts by the approximate z scores (assuming independent rv's as in the ref)
    # Compute the p-value only for the climate-AI link (no sum of dependent random variables)

    StdDevsagg = dict()
    StdDevsagg[tuple(['non-climate','non-AI',2019])] = StdDevs[tuple(['ai','other',2019])] + StdDevs[tuple(['ai','green',2019])] +\
                                                      StdDevs[tuple(['other','other',2019])] + StdDevs[tuple(['other','green',2019])]
    StdDevsagg[tuple(['non-climate','AI',2019])] = StdDevs[tuple(['ai','ai',2019])] + StdDevs[tuple(['other','ai',2019])]
    StdDevsagg[tuple(['climate','AI',2019])] = StdDevs[tuple(['green','ai',2019])]
    StdDevsagg[tuple(['climate','non-AI',2019])] = StdDevs[tuple(['green','other',2019])] + StdDevs[tuple(['green','green',2019])]

    ZScoresagg = dict()
    ZScoresagg[tuple(['non-climate','non-AI'])] = (Ctotalagg[tuple(['non-climate','non-AI',2019])] - ECijtagg[tuple(['non-climate','non-AI',2019])])/StdDevsagg[tuple(['non-climate','non-AI',2019])]
    ZScoresagg[tuple(['non-climate','AI'])] = (Ctotalagg[tuple(['non-climate','AI',2019])] - ECijtagg[tuple(['non-climate','AI',2019])])/StdDevsagg[tuple(['non-climate','AI',2019])]
    ZScoresagg[tuple(['climate','AI'])] = (Ctotalagg[tuple(['climate','AI',2019])] - ECijtagg[tuple(['climate','AI',2019])])/StdDevsagg[tuple(['climate','AI',2019])]
    ZScoresagg[tuple(['climate','non-AI'])] = (Ctotalagg[tuple(['climate','non-AI',2019])] - ECijtagg[tuple(['climate','non-AI',2019])])/StdDevsagg[tuple(['climate','non-AI',2019])]

    zscore_climate_AI = ZScoresagg[tuple(['climate','AI'])]
    import scipy
    p_value = scipy.stats.norm.sf(abs(zscore_climate_AI))*2 # two-sided
    print(Ctotalagg)
    print(ZScoresagg)
    print(p_value)
    return [Ctotalagg,ZScoresagg,p_value]


df_CPC_latest_classifications_groupby_subclass = df_CPC_latest.groupby("classification_level1")

# all patents
setup_df_results = []
(counts,residuals,p_value) = counts_and_null_model(citations_with_all_targets_recent)
row = ["All",
       counts[('non-climate','non-AI',2019)],counts[('non-climate','AI',2019)],counts[('climate','non-AI',2019)],counts[('climate','AI',2019)],
       residuals[('non-climate','non-AI')],residuals[('non-climate','AI')],residuals[('climate','non-AI')],residuals[('climate','AI')],
       p_value]
setup_df_results.append(row)

# all climate patents
(counts,residuals,p_value) = counts_and_null_model(citations_with_climate_targets_recent)
row = ["Y02",
       counts[('non-climate','non-AI',2019)],counts[('non-climate','AI',2019)],counts[('climate','non-AI',2019)],counts[('climate','AI',2019)],
       residuals[('non-climate','non-AI')],residuals[('non-climate','AI')],residuals[('climate','non-AI')],residuals[('climate','AI')],
       p_value]
setup_df_results.append(row)

# groups of climate/y02 patents
for ycode in ["Y02A","Y02B","Y02D","Y02E","Y02P","Y02T"]:
    print(ycode)
    ycode_patnums = df_CPC_latest_classifications_groupby_subclass.get_group(ycode).patnum.unique()
    citations_with_climate_targets_recent_ycode = citations_with_climate_targets_recent.query("target in @ycode_patnums").copy(deep=True)
    (counts,residuals,p_value) = counts_and_null_model(citations_with_climate_targets_recent_ycode)

    row = [ycode,
           counts[('non-climate','non-AI',2019)],counts[('non-climate','AI',2019)],counts[('climate','non-AI',2019)],counts[('climate','AI',2019)],
           residuals[('non-climate','non-AI')],residuals[('non-climate','AI')],residuals[('climate','non-AI')],residuals[('climate','AI')],
           p_value]

    setup_df_results.append(row)

colnames = ['target','count0','count1','count2','count3','residual0','residual1','residual2','residual3','p_value']
df_results = pd.DataFrame(setup_df_results,columns=colnames)
df_results.to_csv('output/df_results_plot45.csv',index_label=False)

