import pandas as pd

with open('Data/movie.txt') as f:
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