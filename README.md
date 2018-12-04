# Kmer find

**Not ready for general use**. This is an internal project and is probably not
going to work on your machine.

## Dependencies

- [https://github.com/luispedro/fasta\_reader](https://github.com/luispedro/fasta_reader)
- [https://github.com/luispedro/diskhash](https://github.com/luispedro/diskhash)


If you use conda, then the environment.yml file should provide an environment
with everything and then you can type "make" and, hopefully, everything will
compile.

To build the index, run

./build.kmer-ix.sh test.faa

To query the index, run

python query.py test.faa query.faa

License: MIT
