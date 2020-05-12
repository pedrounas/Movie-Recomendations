import pandas as pd

# Clean up movie titles and IDs

with open('Data/movie.txt', encoding = "ISO-8859-1") as f:
    text = f.read().splitlines()

data = []

for line in text:
    temp = line.split(',')
    word = ''

    for i in range(0, len(temp) -1):
        word += temp[i]
    data.append([word,temp[-1]])

data.pop(0)
df = pd.DataFrame(data, columns=['moviename','movieid'])
df['moviename'].replace({'&amp;': "&",
                        '&#039;': "'",
                        '&#034;': '"'}, inplace=True, regex=True)

df.to_csv('Data/movie.csv', index=False)

# Clean up the ratings file

df = pd.read_csv('Data/Ratings.timed.txt', sep= '	', encoding='utf-16-le')
#df = df.iloc[:, :-1]

df.to_csv('Data/Ratings.csv', index=False)

# Clean up profiles

df = pd.read_csv('Data/profile.txt')
df = df.dropna()
df = df.reset_index(drop=True)

df.to_csv('Data/profile.csv', index=False)