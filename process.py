"""
Request:

I want to create document headers based on the information contained in the file names. Here are a few examples:

filename: BIO.G0.01.1_F_NS
Desired header: <doc ID="BIO.G0.01.1" dis="BIO" level= "G0" gender="F" nativeness="NS">

filename: EDU.G2.01.2_M_NNS
Desired header: <doc ID="EDU.G2.01.2" dis="EDU" level= "G2" gender="M" nativeness="NNS">

filename: POL.G1.04.1_M_NS
Desired header: <doc ID="POL.G1.04.1" dis="POL" level= "G1" gender="M" nativeness="NS">

I also want to add </doc> at the end of each text file.

"""

import os


input_dir = 'input_data'
output_dir = 'output_data'


for root, dirs, files in os.walk(input_dir):
    for file in files:
        if file.endswith(".txt"):  # All text files
            filepath = os.path.join(root, file)
            with open(filepath, 'r') as f:

                # Prep
                text = f.read()

                # Add header
                id = file.split('_')[0]
                dis = file.split('.')[0]
                level = file.split('.')[1]
                gender = file.split('_')[1]
                nativeness = file.split('_')[2]
                output = f'<doc ID="{id}" dis="{dis}" level="{level}" gender="{gender}" nativeness="{nativeness}">\n'

                # Add original body text
                output += text

                # Add closing
                output += "\n</doc>"

                #Remove the .txt after nativeness tag
                output = output.replace('NS.txt', 'NS')

                # Output to file
                output_filepath = filepath.replace(input_dir, output_dir)
                os.makedirs(os.path.dirname(output_filepath), exist_ok=True)
                with open(output_filepath, "w") as f:
                    f.write(output)
