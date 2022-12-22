import pandas as pd
import pylab as plt
import seaborn as sns
sns.set_context("talk",font_scale=0.8)

public_grant_info = pd.read_csv('data/patent_grantyear_ai_climate.csv')

##### PLOT 1a)
plt.figure()
ax=plt.gca()
public_grant_info.query('green_ycodes==1').groupby('grantyear').green_ycodes.sum().plot.line(style='*--',ax=ax,label='climate inventions')
public_grant_info.query('ai==1').groupby('grantyear').ai.sum().plot.line(style='x--',label='ai inventions',ax=ax)
public_grant_info.query('ai==1 and green_ycodes==1').groupby('grantyear').ai_and_green_ycodes.sum().plot.line(style='o--',ax=ax,label='climate and ai')
plt.legend(loc='upper left',prop={'size': 10})
plt.ylabel('Granted patents\n')
plt.xlabel('')
plt.title('Granted patents per year\n')
plt.tight_layout()
plt.show()


##### PLOT 1b)
plt.figure()
ax=plt.gca()
public_grant_info.query('green_ycodes==1').groupby('grantyear').green_ycodes.sum().plot.line(style='*--',ax=ax,label='climate inventions')
public_grant_info.query('ai==1').groupby('grantyear').ai.sum().plot.line(style='x--',label='ai inventions',ax=ax)
public_grant_info.query('ai==1 and green_ycodes==1').groupby('grantyear').ai_and_green_ycodes.sum().plot.line(style='o--',ax=ax,label='climate and ai')
plt.legend(loc='upper left',prop={'size': 10})
plt.ylabel('Granted patents\n')
plt.xlabel('')
plt.title('Granted patents per year\n')
ax.set_yscale('log')
plt.ylim([1,10**5])
plt.tight_layout()
plt.show()

##### PLOT 1c)
df_extract_cpc_ycodes = pd.read_csv('data/df_extract_cpc_ycodes.csv')
lookup_ycode_name = {'Y02D': 'ENERGY-EFFICIENT ICT',
                      'Y02A': 'ADAPTATION TO CLIMATE CHANGE', # 'TECHNOLOGIES FOR ADAPTATION TO CLIMATE CHANGE', 
                      'Y02T': 'TRANSPORTATION', # 'CLIMATE CHANGE MITIGATION TECHNOLOGIES RELATED TO TRANSPORTATION',
                      'Y02E': 'ENERGY TECHNOLOGIES', # 'REDUCTION OF GREENHOUSE GAS [GHG] EMISSIONS, RELATED TO ENERGY GENERATION, TRANSMISSION OR DISTRIBUTION',
                      'Y02B': 'BUILDINGS/HOUSING', # 'CLIMATE CHANGE MITIGATION TECHNOLOGIES RELATED TO BUILDINGS, e.g. HOUSING, HOUSE APPLIANCES OR RELATED END-USER APPLICATIONS',
                      'Y02C': 'CARBON CAPTURE/STORAGE', # 'CAPTURE, STORAGE, SEQUESTRATION OR DISPOSAL OF GREENHOUSE GASES [GHG]',
                      'Y02P': 'PRODUCTION/PROCESSING OF GOODS', #'CLIMATE CHANGE MITIGATION TECHNOLOGIES IN THE PRODUCTION OR PROCESSING OF GOODS',
                      'Y02W': 'WASTEWATER/WASTE MANAGEMENT' #'CLIMATE CHANGE MITIGATION TECHNOLOGIES RELATED TO WASTEWATER TREATMENT OR WASTE MANAGEMENT',
                      }
def lookup_ycode(x):
    if x in lookup_ycode_name.keys():
        return lookup_ycode_name[x]
    else:
        return "UNKNOWN"

y02codes = ['Y02A','Y02B','Y02C','Y02D','Y02E','Y02P','Y02T','Y02W']
toplot = dict()
toplot_ranker = dict()
for y02code in y02codes:
    y02patnums = df_extract_cpc_ycodes.query('cpc_subclass==@y02code').patnum.unique()
    y02entries = public_grant_info.query('patnum in @y02patnums and ai==1')
    y02entries_cp = y02entries.copy(deep=True)
    label = lookup_ycode(y02code)
    pretty_string = r'{:<30} {:>5}'.format(label,y02code)
    print(pretty_string)
    label = pretty_string
    y02entries_cp[label] = 1
    y02entries_cp = y02entries_cp[[label,'grantyear']]

    toplot[label] = y02entries_cp.groupby('grantyear').sum().cumsum()
    toplot_ranker[label] = toplot[label].values[-1][0]

plt.figure()
ax = plt.gca()
markerlist_colorblind = ['o','v','^','s','+','d','x','*']
formatlist_colorblind = ['-','--','-.',':','-','--','-.',':']
i = 0
for label in sorted(toplot_ranker,key=toplot_ranker.get,reverse=True):
    markerstyle_colorblind = markerlist_colorblind[i]+formatlist_colorblind[i]
    i += 1
    print(label,toplot_ranker[label])
    toplot[label].plot.line(style=markerstyle_colorblind,ax=ax)#,subplots=True)
plt.legend(loc='upper left',prop={'size': 10, 'family':'monospace'})
plt.xlabel('')
plt.ylabel('Granted patents\n')
plt.title('Cumulative number of AI patents in classes\n')
plt.tight_layout()
plt.show()


##### PLOT 1d)
y02codes = ['Y02A','Y02B','Y02C','Y02D','Y02E','Y02P','Y02T','Y02W']
toplot = dict()
toplot_ranker = dict()
for y02code in y02codes:
    y02patnums = df_extract_cpc_ycodes.query('cpc_subclass==@y02code').patnum.unique()

    y02entries = public_grant_info.query('patnum in @y02patnums')
    y02entries_cp = y02entries.copy(deep=True)
    label = lookup_ycode(y02code)
    pretty_string = r'{:<30} {:>5}'.format(label,y02code)
    print(pretty_string)
    label = pretty_string
    print(label)
    y02entries_cp[label] = 1
    y02entries_cp = y02entries_cp[[label,'grantyear']]
    dict_grantyear_to_ycode_counts = y02entries_cp.groupby('grantyear').sum().cumsum().to_dict()[label]

    aiy02entries = public_grant_info.query('patnum in @y02patnums and ai==1')
    aiy02entries_cp = aiy02entries.copy(deep=True)
    label = lookup_ycode(y02code)
    pretty_string = r'{:<30} {:>5}'.format(label,y02code)
    print(pretty_string)
    label = pretty_string
    aiy02entries_cp[label] = 1
    aiy02entries_cp = aiy02entries_cp[[label,'grantyear']]
    tab = aiy02entries_cp.groupby('grantyear').sum().cumsum()
    tab['year'] = tab.index.values
    tab['yearly_total'] = tab.year.apply(lambda x: dict_grantyear_to_ycode_counts[x])
    tab['yearly_share_ai_of_ycode'] = tab[label]/tab['yearly_total']
    toplot[label] = tab['yearly_share_ai_of_ycode']*100
    toplot_ranker[label] = toplot[label].values[-1]
plt.figure()
ax = plt.gca()
i = 0
markerlist_colorblind = ['o','v','^','s','+','d','x','*']
formatlist_colorblind = ['-','--','-.',':','-','--','-.',':']
for label in sorted(toplot_ranker,key=toplot_ranker.get,reverse=True):
    markerstyle_colorblind = markerlist_colorblind[i]+formatlist_colorblind[i]
    i += 1
    print(label,toplot_ranker[label])
    toplot[label].plot.line(style=markerstyle_colorblind,ax=ax,label=label)#,subplots=True)
plt.legend(loc='upper left',prop={'size': 8,'family':'monospace'})
plt.xlabel('')
plt.ylabel('Percent of ai patents in area\n')
plt.title('Shares of AI in climate technologies since 1976\n')
plt.tight_layout()
plt.show()



##### PLOT 2
df_long_simple = pd.read_csv('data/df_long_simple.csv')
dict_subclass_to_ai_count_citations = df_long_simple.query('ai==1').groupby('subclass').nunique('target').target.to_dict()
dict_subclass_to_full_count_citations = df_long_simple.groupby('subclass').nunique('target').target.to_dict()
def lookup_subclass_title_citations(x, maxlen=48,maxlines=4):
    from textwrap import wrap
    x2 = lookup_ycode(x).upper()
    return ("\n".join((wrap(x2,maxlen)[0:maxlines+1])))+"\n("+str(dict_subclass_to_ai_count_citations[x])+" ai, out of "+str(dict_subclass_to_full_count_citations[x])+" patents)"
df_long_simple['pretty_title'] = df_long_simple.subclass.apply(lookup_subclass_title_citations)
include_ycodes = ['Y02A','Y02B','Y02C','Y02D','Y02E','Y02P','Y02T','Y02W']
pretty_title_order_ycodes = [lookup_subclass_title_citations(x) for x in include_ycodes]

g = sns.catplot(data=df_long_simple.query('Y02==1'),
                x="n_forward_citations_3y", y="type_ainonai_sentencecase", col="pretty_title",
                kind="box", orient="h", col_wrap=2, height=4, aspect=2,                
                showmeans=True,
                whis=[5,95],fliersize=2,
                order = ["AI","Non-AI"],
                col_order = pretty_title_order_ycodes,
                meanprops={"marker":"s","markerfacecolor":"red", "markeredgecolor":"black"})
def custom(y, **kwargs):
    yq = y.quantile(q=0.99)
    plt.axvline(yq, color="orange", linestyle="dashed", linewidth=2)
g = g.map(custom, "n_forward_citations_3y_1p")
g.set(xscale="log",xlim=[0.3,500])
g.set_titles("{col_name}",size=18)
g.set_axis_labels(x_var="Forward citations, 3-year horizon", y_var="")
g.fig.subplots_adjust(top=0.9)
g.fig.suptitle("Citations to climate patents, between 2010-2019",size=24)
plt.show()



##### PLOT 6
from cycler import cycler

df_experience_vs_breakthroughs = pd.read_csv('data/df_experience_vs_breakthroughs.csv')
df_experience_vs_breakthroughs_yearlyq99s = pd.read_csv('data/df_experience_vs_breakthroughs_yearlyq99s.csv')
df_experience_vs_breakthroughs_ycodes = pd.read_csv('data/df_experience_vs_breakthroughs_ycodes.csv')
df_experience_vs_breakthroughs_yearlyq99s_ycodes = pd.read_csv('data/df_experience_vs_breakthroughs_yearlyq99s_ycodes.csv')

plt.figure(figsize=(12,12))
ax = plt.gca()
colorcycle = cycler(color=['black', 'grey', 'blue', 'red', 'orange', 'green', 'magenta', 'pink'])
plt.gca().set_prop_cycle(colorcycle) 
for target_subclass in df_experience_vs_breakthroughs.target_subclass.unique():
    # q99 per year
    df_experience_q99 = df_experience_vs_breakthroughs_yearlyq99s.query('target_subclass == @target_subclass')
    df_experience_q99.plot(kind='line',x='rank',y='rank_breakthroughs',alpha=0.15,ax=ax,style='o-',markersize=2)
ax.get_legend().remove()
ax.set_xscale('log')
ax.set_yscale('log')

# annotate the large ones
# df_experience_vs_breakthroughs_yearlyq99s.target_subclass.value_counts()
ax.annotate("Electric Digital Data Processing", xy=(48800, 700), xytext=(2200, 900),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),fontsize=12)
ax.annotate("Pictorial communication, Television", xy=(13400, 441), xytext=(300, 700),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),fontsize=12)
ax.annotate("Recognition\nof Data", xy=(44892, 434), xytext=(45000, 200),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),fontsize=12)
ax.annotate("Image Data Processing\nor Generation", xy=(19000, 320), xytext=(400, 330),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),fontsize=12)

for ycode in df_experience_vs_breakthroughs_ycodes.ycode.unique():
    print(ycode)
    # q99 per year
    df_experience_q99 = df_experience_vs_breakthroughs_yearlyq99s_ycodes.query('ycode== @ycode')
    df_experience_q99.plot(kind='line',x='rank',y='rank_breakthroughs',label=ycode,marker='o',markersize=4,linewidth=3,ax=ax)

ax.annotate('ADAPTATION', xy=(577, 31), xytext=(50, 50),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))
ax.annotate('BUILDINGS/HOUSING', xy=(369, 10), xytext=(11, 18),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))
ax.annotate('ENERGY-EFFICIENT ICT', xy=(649, 16), xytext=(649, 4),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))
ax.annotate('ENERGY TECHNOLOGIES', xy=(753, 16), xytext=(2000, 16.5),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))
ax.annotate('PRODUCTION/PROCESSING', xy=(795, 27), xytext=(3500, 28),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))
ax.annotate('TRANSPORT', xy=(871, 44), xytext=(250, 100),
            arrowprops=dict(facecolor='black', shrink=0.05, width=1, headwidth=6),
            fontsize=12,bbox=dict(boxstyle="square", fc="wheat"))

ax.get_legend().remove()
ax.set_xscale('log')
ax.set_yscale('log')

plt.ylabel('AI breakthroughs\n')
plt.xlabel('\nAI patents in technology')
plt.title('AI experience and number of highly cited breakthroughs\n',fontsize=20)
plt.xlim(ax.get_ylim()[0],180000)
plt.ylim(ax.get_ylim()[0],1200)
plt.tight_layout()
plt.show()
