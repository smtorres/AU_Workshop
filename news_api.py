from newsapi import NewsApiClient
import requests
import pandas as pd

# Init
newsapi = NewsApiClient(api_key='1d103db59af347ecae1235c897d93872')

# /v2/top-headlines/sources
sources = newsapi.get_sources()

# Filter sources
id_news = []
id_news_arab = []
for i in sources['sources']:
	if i['language']=='en':
		id_news.append(i['id'])
	elif i['language']=='ar':
		id_news_arab.append(i['id'])
	else:
		print('language: '+i['language'])

all_articles = newsapi.get_everything(q='Trump',
                                      sources=', '.join(str(x) for x in id_news),
                                      from_param='2024-02-06',
                                      to='2024-03-05')

#all_articles_arab = newsapi.get_everything(q="غزة",
 #                                     sources=', '.join(str(x) for x in id_news_arab),
  #                                    from_param='2024-02-05',
   #                                   to='2024-03-04',
    #                                  language="ar")

# Function to download and save image
def download_image(url, filename):
    response = requests.get(url)
    if response.status_code == 200:
        with open(filename, 'wb') as f:
            f.write(response.content)

articles_ls = all_articles['articles']

df = []

# Iterate through the articles and append data to DataFrame
for i in list(range(len(articles_ls))):
	temp_row = {'id': i, 'source_id': articles_ls[i]['source']['id'], 'source_name': articles_ls[i]['source']['name'],
	'author': articles_ls[i]['author'], 'title': articles_ls[i]['title'], 'description': articles_ls[i]['description'],
	'url': articles_ls[i]['url'], 'img_url': articles_ls[i]['urlToImage'], 'publishedAt': articles_ls[i]['publishedAt'],
	'content': articles_ls[i]['content']
	}
	df.append(temp_row)

df = pd.DataFrame(df)

# Save DataFrame to CSV file
df.to_csv("/Users/michelletorres/Dropbox/UCLA/CLASS_MATERIAL/IMAGE_ANALYSIS_WORKSHOP/Data/news_articles_trump.csv", index=False)

# Download and save images
for i, row in df.iterrows():
    image_url = row['img_url']
    image_id = row['id']
    if image_url:
        filename = f"/Users/michelletorres/Dropbox/UCLA/CLASS_MATERIAL/IMAGE_ANALYSIS_WORKSHOP/Images2/image_{i}.jpg"  # You can use a more descriptive filename
        download_image(image_url, filename)
