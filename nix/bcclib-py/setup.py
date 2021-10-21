import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="bcclib-py",
    version="1.0.0",
    author="Samuel Leathers",
    author_email="samuel.leathers@iohk.io",
    description="Library for interacting with bcc-cli in python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/the-blockchain-company/bcc-node",
    py_modules= ["bcclib"],
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache License",
        "Operating System :: OS Independent",
    ],
)
