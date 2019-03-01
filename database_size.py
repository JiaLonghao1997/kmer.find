import argparse
description = '''Script for computating the size of the database at indexing time.'''
def parse_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter, 
                                     description=description)
    parser.add_argument('input_file',  metavar='FASTA',
                        help='fasta file to index')
    parser.add_argument('output_file',  metavar='txt',
                        help='database size')
    return parser.parse_args()


def database_size(input_file, output_file):
    """
    Check the maximal key length in a fasta file.
    :param input_file: path to fasta file
    :return: size of the database
    """
    seq = ''
    databasesize = 0
    with open(input_file, 'r') as fasta:
        for line in fasta:
            line = line.strip('\n')
            if line[0] == '>' and seq == '':
                #process the first line of the input file
                header = line
            elif line[0] != '>':
                #join the line with sequence
                seq = seq + line
            elif (line[0] == '>' and seq != '') or line == '':
                #in subsequent lines starting with '>', count aas.
                #Then re_initialize the header and seq variables for the next record.
                databasesize += len(seq)
                seq = ''
                header = line
        databasesize += len(seq)
    output = open(output_file,"w")
    output.write(f"{databasesize}")
    output.close()

def main():
    args = parse_args()
    ifile = args.input_file
    ofile = args.output_file
    databasesize = database_size(input_file=ifile, output_file=ofile)


if __name__ == '__main__':
    main()
