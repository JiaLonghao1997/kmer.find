def count_aas(FileName):
    fasta_file = open(FileName,'r')
    seq = ''
    length_list = []
    for line in fasta_file:
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
            length_list.append(len(seq))
            seq = ''
            header = line
    length_list.append(len(seq))
    return(length_list)
