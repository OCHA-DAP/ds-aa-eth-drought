# Ethiopia Anticipatory Action: Drought

## Background information

Thie repository contains analysis for the 2024
trigger.

## Directory structure

The code in this repository is organized as follows:

```shell

├── analysis      # Main repository of analytical work
├── src           # Code to run any relevant data acquisition/processing pipelines
|
├── .gitignore
├── README.md
└── requirements.txt

```

## Reproducing this analysis

Create a directory where you would like the data to be stored,
and point to it using an environment variable called
`AA_DATA_DIR`.

Next create a new virtual environment and install the requirements with:

```shell
pip install -r requirements.txt
```

If you would like to instead receive the raw data from our team, please
[contact us](mailto:centrehumdata@un.org).
