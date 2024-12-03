 # commands to run prior to script to set up nltk:
 #pip3 install nltk (in Windows command line, need to set file directory to location of Python "scripts" folder first)
 # python:
 # Had to create 'input_data' folder in Downloads and drop the desired files in there; 'output_data' was created there, then moved to the correct folder in my Documents
 # Use Alt+Shift+e to run a line of code or highlighted code in PyCharm 2021.2.3, Python 3.10

 import nltk
 # nltk.download('punkt')

 import os
 from nltk.tokenize import sent_tokenize

 os.chdir('C:\\Users\\lkjaf\\Downloads')

 input_dir = 'input_data'
 output_dir = 'output_data'

 for root, dirs, files in os.walk(input_dir):
     for file in files:
         # if file == "Psych.Bates.1.txt":  # A specific file
         if file.endswith(".txt"):  # All text files
             filepath = os.path.join(root, file)
             with open(filepath, 'r', encoding="utf8") as f:

                 # Prep
                 text = f.read()
                 output = ""

                 # Remove reserved characters (based on previous analysis of the texts)
                 text = text.replace('i.e. ', 'i.e._')
                 text = text.replace('e.g. ', 'e.g._')
                 text = text.replace('et al. (', 'et al._(')
                 text = text.replace('Prof. ', 'Prof._')
                 text = text.replace('approx. ', 'approx._')
                 text = text.replace('Chpt. ', 'Chpt._')
                 text = text.replace('Ch. ', 'Ch._')
                 text = text.replace('Eq. ', 'Eq._')
                 text = text.replace('Eqs. ', 'Eqs._')
                 text = text.replace('Fig. ', 'Fig._')
                 text = text.replace('FIG. ', 'FIG._')
                 text = text.replace('Figs. ', 'Figs._')
                 text = text.replace('\Wp. ', '\Wp._')
                 text = text.replace('\Wpg. ', '\Wpg._')
                 text = text.replace('. ,', '.,')

                 # Paragraphs
                 paragraphs = enumerate(text.split('\n'))
                 for index_p, p in paragraphs:
                     output += "<p>"

                     # Count sentences in paragraph
                     count_s_in_p = 0
                     count_s_in_p_total = 'REPLACEWITHTOTALCOUNT' + str(index_p)

                     # Sentences
                     for s in sent_tokenize(p):
                         # Ignore titles (defined as 1 sentence paragraphs)
                         if len(sent_tokenize(p)) > 1:
                             # Ignore sentences with less than X characters ("a." or "iii.")
                             if len(s) > 3:
                                 count_s_in_p += 1
                                 output += f"<s_count='{count_s_in_p}'_total_count='{count_s_in_p_total}'>{s}</s>"
                                 #Added underscores for processing in CLAWS
                         else:
                             output += s
                     output += "</p>"

                     # Replace
                     output = output.replace(count_s_in_p_total, str(count_s_in_p))

                     # Add back reserved characters
                     output = output.replace('i.e._', 'i.e. ')
                     output = output.replace('e.g._', 'e.g. ')
                     output = output.replace('et al._(', 'et al. (')
                     output = output.replace('Prof._', 'Prof. ')
                     output = output.replace('approx._', 'approx. ')
                     output = output.replace('Chpt._', 'Chpt. ')
                     output = output.replace('Ch._', 'Ch. ')
                     output = output.replace('Eq._', 'Eq. ')
                     output = output.replace('Eqs._', 'Eqs. ')
                     output = output.replace('Fig._', 'Fig. ')
                     output = output.replace('FIG._', 'FIG. ')
                     output = output.replace('Figs._', 'Figs. ')
                     output = output.replace('\Wp._', '\Wp. ')
                     output = output.replace('\Wpg._', '\Wpg. ')
                     output = output.replace('</s>', '</ss>') #Added this for processing in CLAWS

                 # Output to file
                 output_filepath = filepath.replace(input_dir, output_dir)
                 os.makedirs(os.path.dirname(output_filepath), exist_ok=True)
                 with open(output_filepath, "w") as f:
                     f.write(output)