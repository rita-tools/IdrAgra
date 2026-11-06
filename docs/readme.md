The docs folder holds both the documentation's source (index.md + pages/*.md) and the built html docs (_build/html). 

Edits on the source are reflected on the html build by using this console command (requires a sphinx installation):
`python -m sphinx docs docs/_build/html`
or, equivalently:
`sphinx-build -b html docs docs/_build/html`

Alternatively, if sphinx-autobuild is installed (`python -m pip install sphinx-autobuild`), an automatic rebuild on file save is enabled through:
`python -m sphinx_autobuild docs docs/_build/html`


Running `check_parameters.py` helps in making sure all of the idrara_parameters.txt accepted parameters are included in the parameters page of the documentation, and that they defaults match.\
Additionally, launching it with the `--list` argument makes it print the full list of recognized parameters.


After each successfull commit, ReadTheDocs automatically (see `.readthedocs.yaml` in the repo's root) runs `check_parameters.py` and re-builds the documentation.\
It also builds a pdf version, which can be downloaded on the ReadTheDocs website.