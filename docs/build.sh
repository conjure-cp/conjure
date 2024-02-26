rm -rf myenv
python3 -m venv myenv
source myenv/bin/activate
pip install -r requirements.txt
make conjure-help
make html
make singlehtml
make latexpdf
deactivate
