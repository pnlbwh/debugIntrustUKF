#!/bin/bash

source envpy27.sh
stack build && stack exec pipeline
