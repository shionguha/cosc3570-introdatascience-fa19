#!/usr/bin/env python
# coding: utf-8

# In[2]:


import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
get_ipython().run_line_magic('matplotlib', 'inline')


# In[18]:


data = pd.read_csv("Marzion_006144489.csv")
data['success'] = (data['epa'] > 0)
pbp_rp = data[data.epa != 'NA']
pbp_rp = pbp_rp[(data.play_type == 'pass') | (data.play_type == 'run') | (data.play_type == 'no_play')]
teamsoverallepa = pbp_rp.groupby(['posteam']).epa.mean()
firstpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==1)].groupby(['posteam']).success.mean()
firstpass=pd.DataFrame(firstpass)
firstpass['down'] = 1
firstpass = firstpass.reset_index()
secpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==2)].groupby(['posteam']).success.mean()
secpass= pd.DataFrame(secpass)
secpass['down'] = 2
secpass = secpass.reset_index()
thirdpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==3)].groupby(['posteam']).success.mean()
thirdpass =pd.DataFrame(thirdpass)
thirdpass['down'] = 3
thirdpass = thirdpass.reset_index()
teamsoverall = firstpass.append(secpass, ignore_index = True, sort=True)
teamsoverall = teamsoverall.append(thirdpass, ignore_index = True, sort=True)
teamsoverall = teamsoverall.reset_index()
pbp_first = pbp_rp[pbp_rp.down == 1]
pbp_first['pass'] = (pbp_first['play_type'] == 'pass')
pctfirstpass = pbp_first.groupby(['posteam'])['pass'].mean()
pctfirstpass = pd.DataFrame(pctfirstpass)
pbp_sec = pbp_rp[pbp_rp.down == 2]
pbp_sec['pass'] = (pbp_sec['play_type'] == 'pass')
pctsecondpass = pbp_sec.groupby(['posteam'])['pass'].mean()
pctsecondpass = pd.DataFrame(pctsecondpass)
pbp_third = pbp_rp[pbp_rp.down == 3]
pbp_third['pass'] = (pbp_third['play_type'] == 'pass')
pctthirdpass = pbp_third.groupby(['posteam'])['pass'].mean()
pctthirdpass = pd.DataFrame(pctthirdpass)
pctpass = pctfirstpass.append(pctsecondpass, ignore_index = True, sort=True)
pctpass = pctpass.append(pctthirdpass, ignore_index = True, sort=True)
pctpass = pctpass.reset_index()
teamsoverall = teamsoverall.merge(pctpass, how='inner', on=None, left_on=None, right_on=None, left_index=False, right_index=False, sort=False, suffixes=(False, False), copy=False, indicator=False, validate=None)
teamsoverall


# In[12]:


multiples = sns.FacetGrid(teamsoverall, col='down')
multiples = multiples.map(plt.scatter, 'pass', 'success')


# In[6]:


firstpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==1)].groupby(['posteam']).success.mean()
firstpass=pd.DataFrame(firstpass)
firstpass['down'] = 1
firstpass = firstpass.reset_index()
secpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==2)].groupby(['posteam']).success.mean()
secpass= pd.DataFrame(secpass)
secpass['down'] = 2
secpass = secpass.reset_index()
thirdpass = pbp_rp[(pbp_rp.play_type=='pass') & (pbp_rp.down==3)].groupby(['posteam']).success.mean()
thirdpass =pd.DataFrame(thirdpass)
thirdpass['down'] = 3
thirdpass = thirdpass.reset_index()
teamsoverall = firstpass.append(secpass, ignore_index = True, sort=True)
teamsoverall = teamsoverall.append(thirdpass, ignore_index = True, sort=True)
teamsoverall = teamsoverall.reset_index()
pbp_first = pbp_rp[pbp_rp.down == 1]
pbp_first['pass'] = (pbp_first['play_type'] == 'pass')
firstairyds = pbp_first.groupby(['posteam'])['air_yards'].mean()
firstairyds = pd.DataFrame(firstairyds)
pbp_sec = pbp_rp[pbp_rp.down == 2]
pbp_sec['pass'] = (pbp_sec['play_type'] == 'pass')
secairyds = pbp_sec.groupby(['posteam'])['air_yards'].mean()
secairyds = pd.DataFrame(secairyds)
pbp_third = pbp_rp[pbp_rp.down == 3]
pbp_third['pass'] = (pbp_third['play_type'] == 'pass')
thirdairyds = pbp_third.groupby(['posteam'])['air_yards'].mean()
thirdairyds = pd.DataFrame(thirdairyds)
airyds = firstairyds.append(secairyds, ignore_index = True, sort=True)
airyds = airyds.append(thirdairyds, ignore_index = True, sort=True)
airyds = airyds.reset_index()
teamsoverall = teamsoverall.merge(airyds, how='inner', on=None, left_on=None, right_on=None, left_index=False, right_index=False, sort=False, suffixes=(False, False), copy=False, indicator=False, validate=None)
teamsoverall


# In[8]:


multiples = sns.FacetGrid(teamsoverall, col='down')
multiples = multiples.map(plt.scatter, 'air_yards', 'success')


# In[ ]:




