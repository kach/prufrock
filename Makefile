mlton : prufrepl.mlb prufdict.sml prufcore.sml prufparse.sml prufrepl.sml
	mlton -output prufrock-mlton prufrepl.mlb

mlkit : prufrepl.mlb prufdict.sml prufcore.sml prufparse.sml prufrepl.sml
	mlkit -o prufrock-mlkit prufrepl.mlb

smlnj : prufdict.sml prufcore.sml prufparse.sml prufrepl.sml
	sml prufdict.sml prufcore.sml prufparse.sml prufrepl.sml

polyml : prufdict.sml prufcore.sml prufparse.sml prufrepl.sml
	poly --use "prufdict.sml" --use "prufcore.sml" --use "prufparse.sml" --use "prufrepl.sml"
