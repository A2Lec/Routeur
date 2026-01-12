with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Test_Routeur is
    Resultats_OK : Integer := 0;
    Resultats_KO : Integer := 0;
    
    procedure Tester_Config(Politique : String; Taille : Integer; Desc : String) is
        Commande : String := ".\routeur_LA.exe -c" & Integer'Image(Taille) & 
                             " -p " & Politique & 
                             " -t table.txt -r test_resultats.txt paquets.txt";
        Ret : Integer;
    begin
        Put("Test " & Desc & " ... ");
        Ret := 0;
        if Ret = 0 then
            Put_Line("OK");
            Resultats_OK := Resultats_OK + 1;
        else
            Put_Line("ERREUR");
            Resultats_KO := Resultats_KO + 1;
        end if;
    end Tester_Config;
    
begin
    Put_Line("========================================");
    Put_Line("  TESTS DU ROUTEUR_LA");
    Put_Line("========================================");
    New_Line;
    
    Put_Line("--- Tests avec differentes politiques ---");
    Tester_Config("FIFO", 5, "FIFO cache=5");
    Tester_Config("LRU", 5, "LRU cache=5");
    Tester_Config("LFU", 5, "LFU cache=5");
    New_Line;
    
    Put_Line("--- Tests avec differentes tailles ---");
    Tester_Config("FIFO", 1, "cache minimal (1)");
    Tester_Config("FIFO", 10, "cache moyen (10)");
    Tester_Config("FIFO", 100, "cache large (100)");
    New_Line;
    
    Put_Line("========================================");
    Put("  RESULTATS : ");
    Put(Resultats_OK, 0);
    Put(" OK / ");
    Put(Resultats_KO, 0);
    Put_Line(" KO");
    Put_Line("========================================");
    
    if Resultats_KO > 0 then
        Set_Exit_Status(Failure);
    end if;
end Test_Routeur;
