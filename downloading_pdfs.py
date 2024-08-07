import os
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin

# URL of the website
url = 'https://www.ilsos.gov/departments/vehicles/statistics/electric/home.html'

# Create a folder to save the downloaded PDFs
os.makedirs('pdfs', exist_ok=True)

# Function to download a single PDF
def download_pdf(pdf_url, folder):
    response = requests.get(pdf_url)
    filename = os.path.join(folder, pdf_url.split('/')[-1])
    with open(filename, 'wb') as f:
        f.write(response.content)
    print(f'Downloaded: {filename}')

# Function to find all PDF links on a webpage
def find_pdf_links(url):
    response = requests.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    pdf_links = []
    for link in soup.find_all('a', href=True):
        href = link['href']
        if href.lower().endswith('.pdf'):
            pdf_links.append(urljoin(url, href))
    return pdf_links

# Find all PDF links on the webpage
pdf_links = find_pdf_links(url)

# Download all PDFs found
for pdf_link in pdf_links:
    download_pdf(pdf_link, 'pdfs')
