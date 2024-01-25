/*------------------------------------------------------------------------
 Module:        E:\C\PROJET\PROJET.C
 Author:        J�r�me Serr�
 Project:       Projet
 State:         1.0
 Creation Date: 29/03/1999
 Description:   Compression/d�compression de fichiers par
                la m�thode de Huffman
------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>

/*----------------------------------------------/
  D�clarations concernant la liste chain�e.
-----------------------------------------------*/

/* D�finition d'une cellule qui contiendra le caract�re
   et le nombre de fois qu'il appara�t dans le fichier. */
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
  D�claration concernant l'arbre
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
  D�claration concernant les fichiers
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
 D�claration des fonctions
------------------------------------------------*/

void AfficheAide(void)
{

/* Cette fonction affiche l'aide � l'�cran. */

	printf("\nProjet version 1.0 \n");
    printf("Mai 1999 (c) J�r�me Serr� \n");
    printf("CNAM - TP VARI. \n\n");
    printf("[Projet <commande> <source> <destination>] \n\n");
    printf("<commande> \n");
    printf(" d : d�compresse le fichier source dans le r�pertoire destination. \n");
    printf(" c : compresse le fichier source dans le fichier destination. \n");
    printf(" ? : Affiche l'aide � l'�cran. \n\n");
    printf("Exemple: \n");
    printf("Projet c tartuf.txt e:/tartuf.huf \n");
    printf("Projet d tartuf.huf c:/theatre/moliere/tartuf.old \n\n");

}

void InitialisationTableau(unsigned long Tableau[])
{

/* Cette fonction a pour but d'initialiser � 0 le tableau. */

	unsigned short i;

	for (i = 0; i <= 256; i++) Tableau[i] = 0;

}

void InitialisationTable(struct Objet Table[])
{

/* Cette fonction initialise � 0 la longueur de la nouvelle s�quence. */

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

/* Cette fonction a pour but d'initialiser � 0 le tableau. */

	unsigned short i;

	for (i = 0; i <= 257; i++) Tampon[i] = 0;

}

void ChargementCaractere(unsigned long Tableau[])
{

/* La fr�quence de chaque caract�re est rang�e dans un tableau d'indice variant de 0 � 256.
   On fait correspondre le rang (Table des caracteres Ascii) du caract�re � l'indice.

   EXEMPLE:
   S'il y a 8 caract�res "P" de rang 80 dans le texte, on aura Tableau(80) = 8. */

	unsigned char Temporaire;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur � l'ouverture du fichier on sort du programme. */
	if (Source == NULL)
	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

	Temporaire = fgetc(Source);

	while (!feof(Source))
	{

		NombreCaractereSource = NombreCaractereSource + 1;

		/* Mise � jour de la fr�quence. */
		Tableau[Temporaire] = Tableau[Temporaire] + 1;

		/* Lecture caract�re par caract�re. */
		Temporaire = fgetc(Source);

	}

	/* insertion du code de fin de traitement. */
	Tableau[256] = 1;
	fclose(Source);

}

void CreationListe(struct Cellule **Liste)
{

/* Cette proc�dure cr�e une liste chain�e � partir du tableau de caract�re.
   Elle ajoute les cellules en t�te de liste. */

	unsigned short i;
	struct Cellule *Temporaire;

	for (i = 0; i <= 256; i++)
	{

		if (Tableau[i] != 0)
		{

			/* Cr�ation de la nouvelle cellule en m�moire. */
			Temporaire = malloc(sizeof(struct Cellule));

			/* L'allocation a �chou�e. */
			if (Temporaire == NULL)
			{

				printf("M�moire satur�e. veuillez fermer quelques applications \n");
				exit(EXIT_FAILURE);

			}

			/* Remplissage de la cellule. */
			Temporaire -> Code = i;
			Temporaire -> Frequence = Tableau[i];
			Temporaire -> FilsGauche = NULL;
			Temporaire -> FilsDroit = NULL;

			/* Insertion en t�te de liste. */
			Temporaire -> Suivant = *Liste;
			*Liste = Temporaire;

		}

	}

}

void TriListeFrequence(struct Cellule **Liste)
{

/* Cette fonction trie une liste chain�e en ordre decroissant
   de fr�quence. */

	/* D�claration des pointeurs. */
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
   Liste1 dont les cellules ont toutes la m�me fr�quence,
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

/* Cette fonction trie une liste chain�e en ordre d�croissant
   de code ASCII. */

	/* D�claration des pointeurs. */
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

/* Cette fonction concat�ne deux listes: Liste et Liste1, elle renvoie
   le resultat dans Liste. */

	struct Cellule *Temp = *Liste;

	while (Temp -> Suivant != NULL)	Temp = Temp -> Suivant;

	Temp -> Suivant = *Liste1;

}

void TriListe(struct Cellule **Liste)
{

/* Cette fonction accepte une liste tri�e par ordre d�croissant de frequence
   et renvoie la liste tri�e par ordre decroissant de frequence
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

/* Cette proc�dure cr�e un arbre � partir d'une liste chain�e.
   chaque p�re pointent sur les deux fils dont la frequence est la moins �lev�e.
   En cas d'�galit� on choisit la cellule dont le code ASCII est le plus
   petit.
   Le p�re contient la somme des fr�quences des deux fils et le code ASCII le
   plus petit. */

	/* D�claration des pointeurs.
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

				/* Cr�ation de la nouvelle cellule en m�moire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a �chou�e. */
				if (Temporaire == NULL)
				{

					printf("M�moire satur�e. veuillez fermer quelques applications \n");
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

				/* Cr�ation de la nouvelle cellule en m�moire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a �chou�e. */
				if (Temporaire == NULL)
				{

					printf("M�moire satur�e. veuillez fermer quelques applications \n");
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

                /* Cr�ation de la nouvelle cellule en m�moire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a �chou�e. */
				if (Temporaire == NULL)
				{

					printf("M�moire satur�e. veuillez fermer quelques applications \n");
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

				/* Cr�ation de la nouvelle cellule en m�moire. */
				Temporaire = malloc(sizeof(struct Cellule));

				/* L'allocation a �chou�e. */
				if (Temporaire == NULL)
				{

					printf("M�moire satur�e. veuillez fermer quelques applications \n");
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

/* Cette fonction cr�e la table de Huffman en parcourant l'arbre en profondeur.
   Par convention on associe la valeur 0 aux arcs de gauche et 1 � ceux
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

/* Cette fonction renvoie le caract�re correspondant �
   la s�quence binaire de 8 bits contenue dans Octet. */

	double Somme = 0;
	unsigned short w;

    for (w = 1; w <= 8; w++) Somme = Somme + Octet[w] * (int)(pow((double)2, (double)(8 - w)));

    return ((int)Somme);

}

void Codage(void)
{

/* Cette fonction code le fichier source et
   �crit le fichier destination (compress�). */

	unsigned char Caractere;
	unsigned short i = 0;

	/* Tampon est vide. */
	unsigned short k = 1;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur � l'ouverture du fichier on sort du programme. */
	if (Source == NULL)

	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

	Destination = fopen(NomFichierDestination[0], "wb");

	/* Si il y a une erreur � l'ouverture du fichier on sort du programme. */
	if (Destination == NULL)

	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierDestination[0]);
		exit(EXIT_FAILURE);

	}

    /* Ecriture du rep�re "Huf". */
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

/* Cette fonction v�rifie que le fichier � d�compresser
   est au format projet. */

	unsigned char Caractere;

	Source = fopen(NomFichierSource[0], "rb");

	/* Si il y a une erreur � l'ouverture du fichier on sort du programme. */
	if (Source == NULL)
	{

		printf("Erreur d'ouverture du fichier %s \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

    Caractere = fgetc(Source);

    /* V�rification du format de fichier. */
    if (Caractere != 'H')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compress�. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

  	Caractere = fgetc(Source);

    /* V�rification du format de fichier. */
    if (Caractere != 'u')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compress�. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

   	Caractere = fgetc(Source);

    /* V�rification du format de fichier. */
    if (Caractere != 'f')
	{

        fclose(Source);
        printf("Le fichier %s n'est pas compress�. \n", NomFichierSource[0]);
		exit(EXIT_FAILURE);

	}

}

void ChargementStructure(unsigned long Tableau[])
{

/* Cette fonction charge dans Tableau la structure
   du fichier original (i.e la fr�quence de chaque caractere). */

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

/* Cette fonction convertit un caract�re en sequence binaire de 8 bits. */

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

/* Cette fonction d�code le fichier compress�
   et ecrit le resultat dans le fichier d�stination. */

	unsigned short Fin = 0;
	struct Cellule *Noeud = Arbre;
	unsigned short j = 1;
	unsigned char Caractere;

    Destination = fopen(NomFichierDestination[0], "wb");

	/* Si il y a une erreur � l'ouverture du fichier on sort du programme. */
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

        printf("L'op�ration s'est termin�e avec succ�s. \n\n");
        printf("Le taux de compression du fichier %s est: ", NomFichierDestination[0]);
        printf( "%d pour-cent. \n\n", (long)fabs(100.0 * (1.0 - (NombreCaractereDestination/(NombreCaractereSource + 1.0)))));
        printf("L'op�ration a dur� %d secondes. \n\n", (TimeFin - TimeDebut)/CLOCKS_PER_SEC);

	}

	/* D�compression. */
    if (*argv[1] == 'd')
	{

    	NomFichierSource[0] = argv[2];
 		NomFichierDestination[0] = argv[3];
        FormatFichier();
        printf("\nD�compression du fichier %s en cours, veuillez patienter...\n\n", NomFichierSource[0]);

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

	    printf("L'op�ration s'est termin�e avec succ�s.\n\n");
        printf("L'op�ration a dur� %d secondes.\n\n", (TimeFin - TimeDebut)/CLOCKS_PER_SEC);

	}

	exit(EXIT_SUCCESS);

}
