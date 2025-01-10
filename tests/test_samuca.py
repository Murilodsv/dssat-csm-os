#!/bin/python3
"""
Python test suite for SAMUCA's simulation experiments
"""
import subprocess
import unittest
import os
import pandas as pd
from pathlib import Path
from tests.util.read_write import *

def run_dssat(exe_path: Path, sim_path: Path, args: list)->subprocess.CompletedProcess:
     """Runs dssat from command line"""
     cwd = os.getcwd()
     os.chdir(sim_path)
     stat = subprocess.run([exe_path] + args, shell=True, capture_output=True)
     os.chdir(cwd)
     return stat

def set_scsam(source_simx_path: Path)->Path:
     """Ensures samuca is being setup in a temporary simulation experiment"""
          
     file_lines = ReadFile(source_simx_path)

     #--- find and replace SMODEL field in SIMULATION CONTROLS with SCSAM
     SMODEL_index = grep_index(file_lines, "SIMULATION CONTROLS", "SMODEL")
     old_line = file_lines[SMODEL_index[0]+1]
     new_line = replace_substring_rjust(old_line, SMODEL_index[1], SMODEL_index[2], "SCSAM",revert_rjust=True)
     file_lines[SMODEL_index[0]+1] = new_line

     test_simx_path = source_simx_path.parent.joinpath("TEST0001"+source_simx_path.suffix) 
     WriteFile(test_simx_path, file_lines)
     return test_simx_path

def run_test(exe_path: Path, sim_path: Path, args: list)->subprocess.CompletedProcess:
     """Put together set_scsam and run_dssat"""

     #--- get the source sim experiment file and create a "TEST0001" to avoid side effects
     source_simx_path = sim_path.joinpath(args[1])
     test_simx_path = set_scsam(source_simx_path)
     
     #--- run the test     
     args[1] = str(test_simx_path.stem+test_simx_path.suffix)
     stat = run_dssat(exe_path, sim_path, args)
     return stat

class test_samuca(unittest.TestCase):

    dssat_path = Path("C:/DSSAT48/")
    exe_path = Path("C:/DSSAT48/dscsm048.exe")
    last_stable_version = "4.8.2.0"

    def test_ESAL1401SCX(self):       

        #--- some last-day results from the previous stable version
        OPG_CTRL = {"@YEAR": 2015,
                    "DOY": 158,
                    "DAP": 328,
                    "TTTOT": 3234.399,
                    "SMFMD": 140.217,
                    "SUCMD": 16.177,
                    "LAIGD": 3.579,
                    "SHTD": 2.827,
                    "RDPD": 120.0}
        
        #--- define target ESAL1401.SCX and arguments
        sim_path = self.dssat_path.joinpath("Sugarcane")
        args = ["C", "ESAL1401.SCX", "1"]

        #--- run dssat
        stat = run_test(self.exe_path, sim_path, args)
        self.assertEqual(stat.stderr.decode("utf-8"), "", f"Simulation ERROR ({stat.stderr.decode("utf-8")}):\n {stat.stdout.decode("utf-8")}")
        
        #--- read outputs
        OPG_path = self.dssat_path.joinpath("Sugarcane/PlantGro.OUT")
        out_head = pd.read_csv(OPG_path, skiprows=4, nrows=5, sep=':', header=None, engine="python")
        out_data = pd.read_csv(OPG_path, skiprows=13, sep='\\s+', header=0, engine="python")
        
        #--- check meta info
        sim_exp = out_head.loc[0,1][36:44]+".SCX"
        model   = out_head.loc[0,1][27:32]
        self.assertEqual(sim_exp, "TEST0001.SCX", f"The output is for {sim_exp} but we expected TEST0001.SCX. Check test suite and functions.")
        self.assertEqual(model, "SCSAM", f"The model used in {model}, but we expected SCSAM for samuca.")

        #--- check data values
        OPG_LASTDAY = out_data.loc[len(out_data)-1]
        for v in OPG_CTRL:
             self.assertIn(v, OPG_LASTDAY, f"Variable {v} not found in output file {OPG_path}")
             #self.assertAlmostEqual(OPG_LASTDAY[v], OPG_CTRL[v], places=len(str(OPG_CTRL[v]).split(".")[1]), msg=f"Value for variable {v} differs:\n before={OPG_CTRL[v]}\n now={OPG_LASTDAY[v]}")            
             self.assertEqual(OPG_LASTDAY[v], OPG_CTRL[v], f"Value for variable {v} differs:\n before={OPG_CTRL[v]}\n now={OPG_LASTDAY[v]}")            


        

        
