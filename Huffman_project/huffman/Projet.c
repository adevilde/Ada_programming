/*------------------------------------------------------------------------
 Module:        E:\C\PROJET\PROJET.C
 Author:        Jérôme Serré
 Project:       Projet
 State:         1.0
 Creation Date: 29/03/1999
 Description:   Compression/décompression de fichiers par
                la méthode de Huffman
------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>

/*----------------------------------------------/
  Déclarations concernant la liste chainée.
-----------------------------------------------*/

/* Définition d'une cellule qui contiendra le caractère
   et le nombre de fois qu'il apparaît dans le fichier. */
struct Cellule
{
	unsigned short Code;
    unsigned long Frequence;
    struct Cellule *Suivant;
    struct Cellule *FilsGauche;
    struct Cellule *FilsDroit;
};

struct Cellule *Liste = NULL;
unsigned long Tableau[257];

/*-----------------------------------------------/
  Déclaration concernant l'arbre
------------------------------------------------*/

struct Objet
{
    unsigned short Longueur;
	unsigned short Binaire[257];
};

unsigned short Tampon[257];
struct Cellule *Arbre;
struct Objet CaractereCode[257];


/*-----------------------------------------------/
  Déclaration concernant les fichiers
------------------------------------------------*/

FILE *Source, *Destination;
char *NomFichierSource[1];
char *NomFichierDestination[1];

/*----------------------------------------------/
  Divers
-----------------------------------------------*/

float NombreCaractereSource = 0;
float NombreCaractereDestination = 0;

/*----------------------------------------------/
 Déclaration des fonctions
------------------------------------------------*/

void AfficheAide(void)
{

/* Cette fonction affiche l'aide à l'écran. */

	printf("\nProjet version 1.0 \n");
    printf("Mai 1999 (c) Jérôme Serré \n");
    printf("CNAM - TP VARI. \n\n");
    printf("[Projet <commande> <source> <destination>] \n\n");
    printf("<commande> \n");
    printf(" d : décompresse le fichier source dans le répertoire destination. \n");
    printf(" c : compresse le fichier source dans le fichier destination. \n");
    printf(" ? : Affiche l'aide à l'écran. \n\n");
    printf("Exemple: \n");
    printf("Projet c tartuf.txt e:/tartuf.huf \n");
    printf("Projet d tartuf.huf c:/theatre/moliere/tartuf.old \n\n");

}

void InitialisationTableau(unsigned long Tableau[])
{

/* Cette fonction a pour but d'initialiser à 0 le tableau. */

	unsigned short i;

	for (i = 0; i <= 256; i++) Tableau[i] = 0;

}

void InitialisationTable(struct Objet Table[])
{

/* Cette fonction initialise à 0 la longueur de la nouvelle séquence. */

	unsigned short i, k;

	for (i = 0; i <= 257; i++)
	{

		Table[i].Longueur = 0;

		for (k = 0; k <= 257; k++)
		{

			Table[i].Binaire[k] = 0;

		}

	}

}

void InitialisationTampon(unsigned short Tampon[])
{

/* Cette fonction a pour but d'initialiser à 0 le tableau. */

	unsigned short i;

	for (i = 0; i <= 257; i++) Tampon[i] = 0;

}

void ChargementCaractere(unsigned long Tableau[])
{

/* La fréquence de chaque caractère est rangée dans un tableau d'indice variant de 0 à 256.
   On fait correspondre le rang (Table des caracteres Ascii) du caractère à l'indice.

   EXEMPLE:
   S'il y a 8 caractères "P" de rang 80 dans le texte, on aura Tableau(80) = 8. */

	unsigned char Temporaire;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur à l'ouverture du fichier on sort du programme. */
	if (Source == NULL)
	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

	Temporaire = fgetc(Source);

	while (!feof(Source))
	{

		NombreCaractereSource = NombreCaractereSource + 1;

		/* Mise à jour de la fréquence. */
		Tableau[Temporaire] = Tableau[Temporaire] + 1;

		/* Lecture caractère par caractère. */
		Temporaire = fgetc(Source);

	}

	/* insertion du code de fin de traitement. */
	Tableau[256] = 1;
	fclose(Source);

}

void CreationListe(struct Cellule **Liste)
{

/* Cette procédure crée une liste chainée à partir du tableau de caractère.
   Elle ajoute les cellules en tête de liste. */

	unsigned short i;
	struct Cellule *Temporaire;

	for (i = 0; i <= 256; i++)
	{

		if (Tableau[i] != 0)
		{

			/* Création de la nouvelle cellule en mémoire. */
			Temporaire = malloc(sizeof(struct Cellule));

			/* L'allocation a échouée. */
			if (Temporaire == NULL)
			{

				printf("Mémoire saturée. veuillez fermer quelques applications \n");
				exit(EXIT_FAILURE);

			}

			/* Remplissage de la cellule. */
			Temporaire -> Code = i;
			Temporaire -> Frequence = Tableau[i];
			Temporaire -> FilsGauche = NULL;
			Temporaire -> FilsDroit = NULL;

			/* Insertion en tête de liste. */
			Temporaire -> Suivant = *Liste;
			*Liste = Temporaire;

		}

	}

}

void TriListeFrequence(struct Cellule **Liste)
{

/* Cette fonction trie une liste chainée en ordre decroissant
   de fréquence. */

	/* Déclaration des pointeurs. */
	struct Cellule *Grand, *Petit, *GrandPrecedent, *PetitPrecedent, *PetitSuivant;

	/* Initialisation des pointeurs. */
	Grand = *Liste;
	Petit = Grand -> Suivant;
	GrandPrecedent = NULL;
   	PetitPrecedent = Grand;

   	while (Grand -> Suivant != NULL)
	{

       	while (Petit != NULL)
		{

        	PetitSuivant = Petit -> Suivant;

       		if (Petit -> Frequence > Grand -> Frequence)
			{

           		if (Grand -> Suivant == Petit)
				{

               		if (Grand == *Liste)
					{

                   		Petit -> Suivant = Grand;
                   		*Liste = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		Petit = PetitSuivant;
                   		Grand = *Liste;

               		}

					else
					{

                   		Petit -> Suivant = Grand;
                   		GrandPrecedent -> Suivant = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		Petit = PetitSuivant;
                   		Grand = GrandPrecedent -> Suivant;

					}

           		}

				else
				{

               		if (Grand == *Liste)
					{

                   		Petit -> Suivant = Grand -> Suivant;
                   		*Liste = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		PetitPrecedent -> Suivant = Grand;
                   		Petit = PetitSuivant;
                   		Grand = *Liste;
                   		PetitPrecedent = PetitPrecedent -> Suivant;

					}

                	else
					{

                   		Petit -> Suivant = Grand -> Suivant;
                   		GrandPrecedent -> Suivant = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		PetitPrecedent -> Suivant = Grand;
                   		Petit = PetitSuivant;
                   		Grand = GrandPrecedent -> Suivant;
                   		PetitPrecedent = PetitPrecedent -> Suivant;

               		}

           		}

       		}

			else
			{

               	Petit = Petit -> Suivant;
               	PetitPrecedent = PetitPrecedent -> Suivant;

   			}

    	}

       	if (GrandPrecedent == NULL)	GrandPrecedent = *Liste;

       	else GrandPrecedent = GrandPrecedent -> Suivant;

       	Grand = Grand -> Suivant;
       	Petit = Grand -> Suivant;
       	PetitPrecedent = Grand;

   	}

}

void Scission(struct Cellule **Liste, struct Cellule **Liste1, struct Cellule **Liste2)
{

/* Cette fonction scinde une liste en deux listes:
   Liste1 dont les cellules ont toutes la même fréquence,
   et liste2 le reste de la liste. */

	struct Cellule *Liste2Precedent = *Liste;

   	*Liste1 = *Liste;
   	*Liste2 = (*Liste1)->Suivant;

   	if (*Liste2 != NULL)
	{

    	while ((*Liste1)->Frequence == (*Liste2)->Frequence)
		{

       		if ((*Liste2)->Suivant != NULL)
			{

           		*Liste2 = (*Liste2)->Suivant;
           		Liste2Precedent = Liste2Precedent -> Suivant;

			}

       		else
			{

           		*Liste2 = NULL;
           		break;

			}

    	}

    	if (*Liste2 != NULL) Liste2Precedent -> Suivant = NULL;

	}

}

void TriListeCode(struct Cellule **Liste)
{

/* Cette fonction trie une liste chainée en ordre décroissant
   de code ASCII. */

	/* Déclaration des pointeurs. */
	struct Cellule *Grand, *Petit, *GrandPrecedent, *PetitPrecedent, *PetitSuivant;

	/* Initialisation des pointeurs. */
	Grand = *Liste;
	Petit = Grand -> Suivant;
	GrandPrecedent = NULL;
	PetitPrecedent = Grand;

	while (Grand -> Suivant != NULL)
	{

   		while (Petit != NULL)
		{

       		PetitSuivant = Petit -> Suivant;

       		if (Petit -> Code > Grand -> Code)
			{

       			if (Grand -> Suivant == Petit)
				{

               		if (Grand == *Liste)
					{

                   		Petit -> Suivant = Grand;
                   		*Liste = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		Petit = PetitSuivant;
                   		Grand = *Liste;

					}

               		else
					{

                   		Petit -> Suivant = Grand;
                   		GrandPrecedent -> Suivant = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		Petit = PetitSuivant;
                   		Grand = GrandPrecedent -> Suivant;

           			}

				}

           		else
				{

               		if (Grand == *Liste)
					{

                   		Petit -> Suivant = Grand -> Suivant;
                   		*Liste = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		PetitPrecedent -> Suivant = Grand;
                   		Petit = PetitSuivant;
                   		Grand = *Liste;
                   		PetitPrecedent = PetitPrecedent -> Suivant;

					}

               		else
					{

                   		Petit -> Suivant = Grand -> Suivant;
                   		GrandPrecedent -> Suivant = Petit;
                   		Grand -> Suivant = PetitSuivant;
                   		PetitPrecedent -> Suivant = Grand;
                   		Petit = PetitSuivant;
                   		Grand = GrandPrecedent -> Suivant;
                   		PetitPrecedent = PetitPrecedent -> Suivant;

           			}

       			}

			}

       		else
			{

           		Petit = Petit -> Suivant;
           		PetitPrecedent = PetitPrecedent -> Suivant;

			}

		}

   		if (GrandPrecedent == NULL) GrandPrecedent = *Liste;

   		else GrandPrecedent = GrandPrecedent -> Suivant;

   		Grand = Grand -> Suivant;
   		Petit = Grand -> Suivant;
   		PetitPrecedent = Grand;

	}

}

void Concatenation(struct Cellule **Liste, struct Cellule **Liste1)
{

/* Cette fonction concaténe deux listes: Liste et Liste1, elle renvoie
   le resultat dans Liste. */

	struct Cellule *Temp = *Liste;

	while (Temp -> Suivant != NULL)	Temp = Temp -> Suivant;

	Temp -> Suivant = *Liste1;

}

void TriListe(struct Cellule **Liste)
{

/* Cette fonction accepte une liste triée par ordre décroissant de frequence
   et renvoie la liste triée par ordre decroissant de frequence
   et de code ASCII. */

	struct Cellule *ListeGauche = NULL;
	struct Cellule *ListeDroite = NULL;
	struct Cellule *Temporaire = NULL;

	TriListeFrequence(Liste);
    Scission(Liste, &ListeGauche, &ListeDroite);
    TriListeCode(&ListeGauche);
    *Liste = ListeGauche;
    Temporaire = ListeDroite;

    while (ListeDroite != NULL)
	{

        Scission(&Temporaire, &ListeGauche, &ListeDroite);
        TriListeCode(&ListeGauche);
        Concatenation(Liste, &ListeGauche);
        Temporaire = ListeDroite;

	}

}

void CreationArbre(struct Cellule **Liste, struct Cellule **Arbre)
{

/* Cette procédure crée un arbre à partir d'une liste chainée.
   chaque pére pointent sur les deux fils dont la frequence est la moins élevée.
   En cas d'égalité on choisit la cellule dont le code ASCII est le plus
   petit.
   Le pére contient la somme des fréquences des deux fils et le code ASCII le
   plus petit. */

	/* Déclaration des pointeurs.
	CC:cellule en cours. CP:cellule precedente. CPP:cellule precedant la precedente. */
	struct Cellule *CC, *CP, *CPP, *Temporaire;

    while ((*Liste)->Suivant != NULL)
	{

        CC = (*Liste)->Suivant;
        CP = *Liste;
        CPP = NULL;

        while (CC->Suivant != NULL)
		{

            CC = CC->Suivant;
            CP = CP->Suivant;

            if (CPP == NULL) CPP = *Liste;

            else CPP = CPP->Suivant;

        }

        if (CC->Code < CP->Code)
		{

            if (CPP != NULL)
			{

				/* Création de la nouvelle cellule en mémoire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a échouée. */
				if (Temporaire == NULL)
				{

					printf("Mémoire saturée. veuillez fermer quelques applications \n");
					exit(EXIT_FAILURE);

				}

				/* Remplissage de la cellule. */
				Temporaire->Code = CC->Code;
				Temporaire->Frequence = CC->Frequence + CP->Frequence;
				Temporaire->FilsGauche = CP;
				Temporaire->FilsDroit = CC;
				Temporaire->Suivant = NULL;

        		CPP->Suivant = Temporaire;

           	}

            else
            {

				/* Création de la nouvelle cellule en mémoire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a échouée. */
				if (Temporaire == NULL)
				{

					printf("Mémoire saturée. veuillez fermer quelques applications \n");
					exit(EXIT_FAILURE);

				}

				/* Remplissage de la cellule. */
				Temporaire->Code = CC->Code;
				Temporaire->Frequence = CC->Frequence + CP->Frequence;
				Temporaire->FilsGauche = CP;
				Temporaire->FilsDroit = CC;
				Temporaire->Suivant = NULL;
                *Liste = Temporaire;

           	}

		}

        else
        {

            if (CPP != NULL)
			{

                /* Création de la nouvelle cellule en mémoire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a échouée. */
				if (Temporaire == NULL)
				{

					printf("Mémoire saturée. veuillez fermer quelques applications \n");
					exit(EXIT_FAILURE);

				}

				/* Remplissage de la cellule. */
				Temporaire->Code = CP->Code;
				Temporaire->Frequence = CC->Frequence + CP->Frequence;
				Temporaire->FilsGauche = CP;
				Temporaire->FilsDroit = CC;
				Temporaire->Suivant = NULL;

        		CPP->Suivant = Temporaire;

			}

            else
            {

				/* Création de la nouvelle cellule en mémoire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a échouée. */
				if (Temporaire == NULL)
				{

					printf("Mémoire saturée. veuillez fermer quelques applications \n");
					exit(EXIT_FAILURE);

				}

				/* Remplissage de la cellule. */
				Temporaire->Code = CP->Code;
				Temporaire->Frequence = CC->Frequence + CP->Frequence;
				Temporaire->FilsGauche = CP;
				Temporaire->FilsDroit = CC;
				Temporaire->Suivant = NULL;
                *Liste = Temporaire;

			}

    	}

        CP->Suivant = NULL;
        TriListe(Liste);

	}

    *Arbre = *Liste;

}

unsigned short EstFeuille(struct Cellule *Noeud)
{

/* Cette fonction renvoie 1 si le noeud est une feuille
   de l'arbre, 0 sinon. */

    if (Noeud->FilsGauche == NULL && Noeud->FilsDroit == NULL) return(1);

    else return(0);

}

void CreationTable(struct Cellule *Arbre)
{

/* Cette fonction crée la table de Huffman en parcourant l'arbre en profondeur.
   Par convention on associe la valeur 0 aux arcs de gauche et 1 à ceux
   de droite. */

	static unsigned short Hauteur = 1;
	unsigned short k;

	if (EstFeuille(Arbre))
	{

        for (k = 1; k <= (Hauteur - 1); k++) (CaractereCode[Arbre->Code]).Binaire[k] = Tampon[k];

        (CaractereCode[Arbre->Code]).Longueur = Hauteur - 1;

	}

    else
    {

       	Tampon[Hauteur] = 0;
        Hauteur = Hauteur + 1;
        CreationTable(Arbre->FilsGauche);

        Tampon[Hauteur] = 1;
        Hauteur = Hauteur + 1;
        CreationTable(Arbre->FilsDroit);

	}

    Hauteur = Hauteur - 1;

}

unsigned char BinaireDecimale(unsigned short Octet[])
{

/* Cette fonction renvoie le caractère correspondant à
   la séquence binaire de 8 bits contenue dans Octet. */

	double Somme = 0;
	unsigned short w;

    for (w = 1; w <= 8; w++) Somme = Somme + Octet[w] * (int)(pow((double)2, (double)(8 - w)));

    return ((int)Somme);

}

void Codage(void)
{

/* Cette fonction code le fichier source et
   écrit le fichier destination (compressé). */

	unsigned char Caractere;
	unsigned short i = 0;

	/* Tampon est vide. */
	unsigned short k = 1;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur à l'ouverture du fichier on sort du programme. */
	if (Source == NULL)

	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

	Destination = fopen(NomFichierDestination[0], "wb");

	/* Si il y a une erreur à l'ouverture du fichier on sort du programme. */
	if (Destination == NULL)

	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierDestination[0]);
		exit(EXIT_FAILURE);

	}

    /* Ecriture du repére "Huf". */
    fputc('H', Destination);
    fputc('u', Destination);
    fputc('f', Destination);
    NombreCaractereDestination = 3;

    /* Ecriture de la structure du fichier source. */
    for (i = 0; i <= 256; i++)
	{

        fprintf(Destination, "%s%d", " ", Tableau[i]);
        NombreCaractereDestination = NombreCaractereDestination + 4;

	}

	/* Codage. */
	Caractere = fgetc(Source);

	while (!feof(Source))
	{

        i = 1;

        while (i <= CaractereCode[Caractere].Longueur)
		{

            if (k > 8)
			{

            	fputc(BinaireDecimale(Tampon), Destination);

                NombreCaractereDestination = NombreCaractereDestination + 1;
                k = 1;

        	}

            Tampon[k] = CaractereCode[Caractere].Binaire[i];
            k++;
            i++;

    	}

		Caractere = fgetc(Source);

	}

    i = 1;

    while (i <= CaractereCode[256].Longueur)
	{

        if (k > 8)
		{

        	fputc(BinaireDecimale(Tampon), Destination);
            NombreCaractereDestination = NombreCaractereDestination + 1;
            k = 1;

    	}

        Tampon[k] = CaractereCode[256].Binaire[i];
        k++;
        i++;

	}

    if (k <= 8)
	{

        for (i = k; i <= 8; i++) Tampon[i] = 0;

	}

    fputc(BinaireDecimale(Tampon), Destination);
    NombreCaractereDestination = NombreCaractereDestination + 1;

    fclose(Source);
    fclose(Destination);

}

void FormatFichier(void)
{

/* Cette fonction vérifie que le fichier à décompresser
   est au format projet. */

	unsigned char Caractere;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur à l'ouverture du fichier on sort du programme. */
	if (Source == NULL)
	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

    Caractere = fgetc(Source);

    /* Vérification du format de fichier. */
    if (Caractere != 'H')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compressé. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

  	Caractere = fgetc(Source);

    /* Vérification du format de fichier. */
    if (Caractere != 'u')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compressé. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

   	Caractere = fgetc(Source);

    /* Vérification du format de fichier. */
    if (Caractere != 'f')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compressé. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

}

void ChargementStructure(unsigned long Tableau[])
{

/* Cette fonction charge dans Tableau la structure
   du fichier original (i.e la fréquence de chaque caractere). */

	unsigned char Caractere[10];
	unsigned short i, j = 0;

	Caractere[0] = fgetc(Source);

	for (i = 0; i <= 255; i++)
	{

    	Caractere[j] = fgetc(Source);

        while (Caractere[j] != ' ')
		{

			j++;
            Caractere[j] = fgetc(Source);

    	}

		j = 0;
		Tableau[i] = atoi(Caractere);

	}

	Caractere[0] = fgetc(Source);
	Tableau[i] = 1;

}

void DecimaleBinaire(unsigned char Caractere, unsigned short Tampon[])
{

/* Cette fonction convertit un caractère en sequence binaire de 8 bits. */

	unsigned short w, i = 9;

    while (Caractere != 0)
	{

	   	i--;
        Tampon[i] = Caractere % 2;
        Caractere = Caractere / 2;

	}

    if (i > 1)
	{

		for (w = 1; w <= (i - 1); w++) Tampon[w] = 0;

    }

}

void Decodage(struct Cellule *Arbre)
{

/* Cette fonction décode le fichier compressé
   et ecrit le resultat dans le fichier déstination. */

	unsigned short Fin = 0;
	struct Cellule *Noeud = Arbre;
	unsigned short j = 1;
	unsigned char Caractere;

    Destination = fopen(NomFichierDestination[0], "wb");

	/* Si il y a une erreur à l'ouverture du fichier on sort du programme. */
	if (Destination == NULL)
	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierDestination[0]);
		exit(EXIT_FAILURE);

	}

    Caractere = fgetc(Source);
    DecimaleBinaire(Caractere, Tampon);

    while (!Fin)
	{

    	while (!EstFeuille(Noeud))
	    {

        	if (Tampon[j] == 0) Noeud = Noeud->FilsGauche;

           	else Noeud = Noeud->FilsDroit;

            j++;

            if (j == 9)
			{

            	if (!feof(Source))
				{

                	Caractere = fgetc(Source);
                    DecimaleBinaire(Caractere, Tampon);
                    j = 1;
				}

            }

   		}

        if (Noeud->Code == 256) Fin = 1;

        else
		{

            fprintf(Destination, "%c", Noeud->Code);
            Noeud = Arbre;

    	}

	}

    fclose(Source);
    fclose(Destination);

}

/* Corps du programme. */
void main(int argc, char *argv[])
{

	unsigned long TimeDebut, TimeFin;

    if (argc != 4)
	{

		AfficheAide();
		exit(EXIT_SUCCESS);

	}

	if (*argv[1] != 'c' && *argv[1] != 'd')
	{

		AfficheAide();
		exit(EXIT_SUCCESS);

	}

    /* Compression. */
    if (*argv[1] == 'c')
	{

    	NomFichierSource[0] = argv[2];
		NomFichierDestination[0] = argv[3];

        printf("\nCompression du fichier %s en cours, veuillez patienter... \n\n", NomFichierSource[0]);

        TimeDebut = clock();

    	InitialisationTableau(Tableau);
		InitialisationTable(CaractereCode);
		InitialisationTampon(Tampon);
		ChargementCaractere(Tableau);
		CreationListe(&Liste);
		TriListe(&Liste);
		CreationArbre(&Liste, &Arbre);
		CreationTable(Arbre);
    	Codage();
		free(Liste);
		free(Arbre);

        TimeFin = clock();

        printf("L'opération s'est terminée avec succès. \n\n");
        printf("Le taux de compression du fichier %s est: ", NomFichierDestination[0]);
        printf( "%d pour-cent. \n\n", (long)fabs(100.0 * (1.0 - (NombreCaractereDestination/(NombreCaractereSource + 1.0)))));
        printf("L'opération a duré %d secondes. \n\n", (TimeFin - TimeDebut)/CLOCKS_PER_SEC);

	}

	/* Décompression. */
    if (*argv[1] == 'd')
	{

    	NomFichierSource[0] = argv[2];
 		NomFichierDestination[0] = argv[3];
        FormatFichier();
        printf("\nDécompression du fichier %s en cours, veuillez patienter...\n\n", NomFichierSource[0]);

		TimeDebut = clock();

		InitialisationTableau(Tableau);
		InitialisationTable(CaractereCode);
		InitialisationTampon(Tampon);
        ChargementStructure(Tableau);
        CreationListe(&Liste);
    	TriListe(&Liste);
    	CreationArbre(&Liste, &Arbre);
        Decodage(Arbre);
		free(Liste);
		free(Arbre);

        TimeFin = clock();

	    printf("L'opération s'est terminée avec succès.\n\n");
        printf("L'opération a duré %d secondes.\n\n", (TimeFin - TimeDebut)/CLOCKS_PER_SEC);

	}

	exit(EXIT_SUCCESS);

}
