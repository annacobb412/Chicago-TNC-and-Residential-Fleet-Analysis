import PyPDF2
import os

def pdf_to_text(pdf_path, output_txt):
    # Open the PDF file in read-binary mode
    with open(pdf_path, 'rb') as pdf_file:
        # Create a PdfReader object instead of PdfFileReader
        pdf_reader = PyPDF2.PdfReader(pdf_file)

        # Initialize an empty string to store the text
        text = ''

        for page_num in range(len(pdf_reader.pages)):
            page = pdf_reader.pages[page_num]
            text += page.extract_text()

    # Write the extracted text to a text file
    with open(output_txt, 'w', encoding='utf-8') as txt_file:
        txt_file.write(text)

if __name__ == "__main__":
    dir_pdf = 'Illinois Hybrid Registrations'
    dir_txt = 'Illinois Hybrid Registrations TXTs'
    pdf_files = os.listdir(dir_pdf)
    counter = 0
    for file in pdf_files:
        txt_file_name = dir_txt + '/' + file[0:-4] + '.txt'
        pdf_to_text(dir_pdf + '/' + file, txt_file_name)
        counter += 1
        print("PDF #" + str(counter) + " converted to text successfully!")
