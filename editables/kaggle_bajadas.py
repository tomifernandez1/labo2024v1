import pandas as pd
from kaggle.api.kaggle_api_extended import KaggleApi

# Configure the Kaggle API
api = KaggleApi()
api.authenticate()

# Specify the competition
competition = "labo-i-2024-virtual"

# Get the list of submissions
submissions = api.competitions_submissions_list(competition)

# Prepare the data for the DataFrame
data = []
for submission in submissions:
    if submission['teamName'] == "Tomas Fernandez":  # Check if the submission is from the specific user
        data.append([submission['ref'], submission['description'], submission['fileName'], submission['publicScore']])

# Create a DataFrame
df = pd.DataFrame(data, columns=['Submission ID', 'File Name', 'Description', 'Public Score'])

# Specify the directory to save the Excel file
directory = r'C:\Users\tomif\Desktop\LABO LAPTOP\guantes blancos\kaggle_bajadas'

# Write the DataFrame to an Excel file in the specified directory
df.to_excel(f'{directory}\\submissions_5.xlsx', index=False)