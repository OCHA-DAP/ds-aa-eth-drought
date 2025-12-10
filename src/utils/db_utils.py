import os
from typing import Literal

from dotenv import load_dotenv
from sqlalchemy import create_engine, insert

load_dotenv()

DSCI_AZ_DB_DEV_PW = os.getenv("DSCI_AZ_DB_DEV_PW")
DSCI_AZ_DB_PROD_PW = os.getenv("DSCI_AZ_DB_PROD_PW")

DSCI_AZ_DB_PROD_UID = os.getenv("DSCI_AZ_DB_PROD_UID")
DSCI_AZ_DB_DEV_UID = os.getenv("DSCI_AZ_DB_DEV_UID")

DSCI_AZ_DB_PROD_HOST = os.getenv("DSCI_AZ_DB_PROD_HOST")
DSCI_AZ_DB_DEV_HOST = os.getenv("DSCI_AZ_DB_DEV_HOST")

AZURE_DB_BASE_URL = "postgresql+psycopg2://{uid}:{pw}@{db_host}/postgres"


def get_engine(stage: Literal["dev", "prod"] = "dev"):
    """
    Create a SQLAlchemy engine for connecting to Azure SQL Database.

    Parameters
    ----------
    stage : Literal["dev", "prod"], optional
        Environment stage to connect to, by default "dev"

    Returns
    -------
    sqlalchemy.engine.Engine
        SQLAlchemy engine configured with the appropriate connection URL

    Raises
    ------
    ValueError
        If the provided stage is neither "dev" nor "prod"
    """
    if stage == "dev":
        url = AZURE_DB_BASE_URL.format(
            uid=DSCI_AZ_DB_DEV_UID,
            pw=DSCI_AZ_DB_DEV_PW,
            db_host=DSCI_AZ_DB_DEV_HOST,
        )
    elif stage == "prod":
        url = AZURE_DB_BASE_URL.format(
            uid=DSCI_AZ_DB_PROD_UID,
            pw=DSCI_AZ_DB_PROD_PW,
            db_host=DSCI_AZ_DB_PROD_HOST,
        )
    else:
        raise ValueError(f"Invalid stage: {stage}")
    return create_engine(url)

