@echo off
findstr /V "\-N" "F24-22_MyExpoP.O.#259_CoA.csv" > "F24-22_MyExpoP.O.#259_CoA-WB.csv"
findstr /V "\-WB" "F24-22_MyExpoP.O.#259_CoA.csv" > "F24-22_MyExpoP.O.#259_CoA-N.csv"

