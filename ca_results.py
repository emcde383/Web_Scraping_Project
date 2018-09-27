from bs4 import BeautifulSoup
import requests
import csv
import pandas as pd

index = 2016
with open("ca_reviews.csv", "w") as csvfile:
    results_writer = csv.writer(csvfile)

    url_list = ["http://www.castatefair.org/california-commercial-wine-competition-results/",
                "http://www.castatefair.org/california-commercial-wine-competition-results-17/",
                "http://www.castatefair.org/california-commercial-wine-competition-results-18/"]
     
    results = pd.DataFrame({})
    for url in url_list:
    
        text = requests.get(url).text
        text = BeautifulSoup(text, 'html.parser')
        
        if url == "http://www.castatefair.org/california-commercial-wine-competition-results-18/":
    
            wineryTag = text.find_all("td", {"class": "column-2"})
            wineryString = [tag.string for tag in wineryTag]
            
            vintageTag = text.find_all("td", {"class": "column-3"})
            vintageString = [tag.string for tag in vintageTag]
            
            winenameTag = text.find_all("td", {"class": "column-4"})
            winenameString = [tag.string for tag in winenameTag]
            
            grapeTag = text.find_all("td", {"class": "column-5"})
            grapeString = [tag.string for tag in grapeTag]
            
            regionTag = text.find_all("td", {"class": "column-6"})
            regionString = [tag.string for tag in regionTag]
            
            appelationTag = text.find_all("td", {"class": "column-7"})
            appelationString = [tag.string for tag in appelationTag]
            
            specialawardTag = text.find_all("td", {"class": "column-8"})
            specialawardString = [tag.string for tag in specialawardTag]
            
            awardTag = text.find_all("td", {"class": "column-9"})
            awardString = [tag.string for tag in awardTag]
            
            scoreTag = text.find_all("td", {"class": "column-10"})
            scoreString = [tag.string for tag in scoreTag]
            
            priceTag = text.find_all("td", {"class": "column-11"})
            priceString = [tag.string for tag in priceTag]
        
        else:
            
            wineryTag = text.find_all("td", {"class": "column-1"})
            wineryString = [tag.string for tag in wineryTag]
            
            vintageTag = text.find_all("td", {"class": "column-2"})
            vintageString = [tag.string for tag in vintageTag]
            
            winenameTag = text.find_all("td", {"class": "column-3"})
            winenameString = [tag.string for tag in winenameTag]
            
            grapeTag = text.find_all("td", {"class": "column-4"})
            grapeString = [tag.string for tag in grapeTag]
            
            regionTag = text.find_all("td", {"class": "column-5"})
            regionString = [tag.string for tag in regionTag]
            
            appelationTag = text.find_all("td", {"class": "column-6"})
            appelationString = [tag.string for tag in appelationTag]
            
            awardTag = text.find_all("td", {"class": "column-7"})
            awardString = [tag.string for tag in awardTag]
            
            scoreTag = text.find_all("td", {"class": "column-8"})
            scoreString = [tag.string for tag in scoreTag]
            
            #account for reording of columns across years
            if url == "http://www.castatefair.org/california-commercial-wine-competition-results/":
                specialawardTag = text.find_all("td", {"class": "column-9"})
                specialawardString = [tag.string for tag in specialawardTag]
                
                priceTag = text.find_all("td", {"class": "column-10"})
                priceString = [tag.string for tag in priceTag]
            
            else:
                priceTag = text.find_all("td", {"class": "column-9"})
                priceString = [tag.string for tag in priceTag]
                
        dic = {}
        dic["winery"] = wineryString
        dic["vintage"] = vintageString
        dic["winename"] = winenameString
        dic["grape"] = grapeString
        dic["region"] = regionString
        dic["appelation"] = appelationString
        dic["award"] = awardString
        dic["score"] = scoreString
        dic["price"] = priceString
        dic["competition_year"] = index
        
        df = pd.DataFrame(dic)
        results = results.append(df)

        print("Finished year " + str(index))
        index = index + 1

    results = results.append(df)
    results.to_csv("./data/output/ca_reviews.csv")