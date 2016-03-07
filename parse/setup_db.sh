#!/bin/bash

createdb reddit

# run a schema file
psql reddit -f db_schema.sql 
