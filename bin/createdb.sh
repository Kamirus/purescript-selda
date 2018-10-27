#!/usr/bin/env bash

export DB=selda

if [ "$1" == "R" ]; then
    dropdb "$DB"
    createdb "$DB"
fi
psql -d "$DB" < sql/createdb.sql
