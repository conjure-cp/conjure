rm -rf myenv
python3 -m venv myenv
source myenv/bin/activate
pip install -r requirements.txt
make html
deactivate
