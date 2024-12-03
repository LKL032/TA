#Doesn't work, could play around with it

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

# Clean up CLAWS tags
output = output.replace('_total_count', ' total_count')
output = output.replace('<s_count', '<s count')
output = output.replace('</ss>	NULL	PUNC', '</s>')
output = output.replace('<p>	NULL	PUNC', '<p>')
output = output.replace('</p>	NULL	PUNC', '</p>')

# Output to file
                output_filepath = filepath.replace(input_dir, output_dir)
                os.makedirs(os.path.dirname(output_filepath), exist_ok=True)
                with open(output_filepath, "w") as f:
                    f.write(output)