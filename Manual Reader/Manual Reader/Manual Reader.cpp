// Manual Reader.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sys/stat.h>
#include <iomanip>

using namespace std;


//Function to check if a file exists or not
bool fileExists(const std::string& filename)
{
	struct stat buf;
	if (stat(filename.c_str(), &buf) != -1)
	{
		return true;
	}
	return false;
}

int main()
{

	char anotherfile = '1';
	bool testtrain;

	cout << "Will you be processing testing or training data? (1 for testing, 0 for training): ";
	cin >> testtrain;
	cout << "\n\n";

	do {
	
		char * tempblock;
		char * fourblock;
		char yn;
		double acount = 0, ascount = 0, bcount = 0, ccount = 0, cscount = 0, dcount = 0, dscount = 0, ecount = 0, fcount = 0, fscount = 0, gcount = 0, gscount = 0;
		double aprop = 0, asprop = 0, bprop = 0, cprop = 0, csprop = 0, dprop = 0, dsprop = 0, eprop = 0, fprop = 0, fsprop = 0, gprop = 0, gsprop = 0;
		int * notearray;
		int fileopen = 0;
		int basenote = 12;
		int majorminor = 12;
		int key = 0;
		int notecount = 0;
		bool notenext = 0;
		bool pastheader = 0;
		bool pasttrack = 0;
		std::string instring;
		string input;
		string output;
		string temp;
		streampos size;
		ifstream myfile;
		ofstream outfile;

		bool atend = 0;

		//Input file section
		do {

			cin.ignore();

			//Suggested name: "AC_DC_-_Thunderstruck - B Major"
			cout << "Enter a file to analyze: ";
			getline(cin, input);

			input = "Finished MIDI\\" + input + ".mid";

			myfile.open(input, ios::in | ios::ate | ios::binary);

			if (!(myfile.is_open())) {
				cout << "\nIncorrect file name. Try Again\n\n";
			}

		} while (!(myfile.is_open()));

		cout << "\n";

		if (myfile.is_open()) {

			fileopen = 1;

			//Make size the size of the file in bytes
			size = myfile.tellg();

			//Initialize character arrays
			tempblock = new char[2];
			tempblock[1] = '\0';

			fourblock = new char[5];
			fourblock[4] = '\0';


			//Move file pointer to first byte in file
			myfile.seekg(0, ios::beg);

			notearray = new int[size];
			
			int length;

			

			do {
					if (!pastheader) {

						myfile.read(fourblock, 4);
						temp = fourblock;

						if (temp == "MThd") {

							pastheader = 1;
							cout << "Found Header\n";

							myfile.read(fourblock, 4);

							length = int((unsigned char)(fourblock[0]) << 24 |
								(unsigned char)(fourblock[1]) << 16 |
								(unsigned char)(fourblock[2]) << 8 |
								(unsigned char)(fourblock[3]));

							//Move pointer to the end of the header
							myfile.seekg(length, ios::cur);
						}
					}
					else if (!pasttrack) {
						myfile.read(fourblock, 4);
						temp = fourblock;
						if (temp == "MTrk") {

							pasttrack = 1;

							//Skip the track length bytes
							myfile.read(fourblock, 4);

							length = int((unsigned char)(fourblock[0]) << 24 |
								(unsigned char)(fourblock[1]) << 16 |
								(unsigned char)(fourblock[2]) << 8 |
								(unsigned char)(fourblock[3]));

							for (int i = 0; i < length; i++) {

								myfile.read(tempblock, 1);

								int b = tempblock[0];

								//x = first 4 binary digits of b, appended with 0000
								unsigned int x = b & 0xF0;

								if (notenext) {

									myfile.read(tempblock, 1);
									int c = tempblock[0];
									i++;

									//Add the note to notearray if the velocity data byte is not set to 0
									if (c != 0) {
										notearray[notecount] = b;
										notenext = 0;
										notecount++;
									}

								}

								//If note is not next, and x is 144 (int of 10010000, status byte for MIDI "Note on" message), set note next to true
								else if (x == 144) {

									notenext = 1;

								}

							}

							pasttrack = 0;

						}
						else {
							atend = 1;
						}

					}

				} while (!atend);

			
			//Print note count
			cout << "The number of notes is " << notecount << "\n";

			//Loop through every filled space of notearray and print its value
			for (int i = 0; i < notecount; i++) {

				//Middle C is note 60, but C is the note 3, so add 3 to the note value before taking remainder from 12 to make all note values correct
				int note = abs((notearray[i]) + 3) % 12;

				//Increment note count based on which note is in notearray[i]
				switch (note) {
				case 0:
					acount++;
					break;
				case 1:
					ascount++;
					break;
				case 2:
					bcount++;
					break;
				case 3:
					ccount++;
					break;
				case 4:
					cscount++;
					break;
				case 5:
					dcount++;
					break;
				case 6:
					dscount++;
					break;
				case 7:
					ecount++;
					break;
				case 8:
					fcount++;
					break;
				case 9:
					fscount++;
					break;
				case 10:
					gcount++;
					break;
				case 11:
					gscount++;
					break;
				}
			}

			cout << "Count of each note: \n";

			//Output count of each note
			cout << "A: " << acount << "\nA#: " << ascount
				<< "\nB: " << bcount << "\nC: " << ccount
				<< "\nC#: " << cscount << "\nD: " << dcount
				<< "\nD#: " << dscount << "\nE: " << ecount
				<< "\nF: " << fcount << "\nF#: " << fscount
				<< "\nG: " << gcount << "\nG#: " << gscount
				<< "\n\n";

			//Normalize the data to a proportion of the total notes
			aprop = acount / notecount;
			asprop = ascount / notecount;
			bprop = bcount / notecount;
			cprop = ccount / notecount;
			csprop = cscount / notecount;
			dprop = dcount / notecount;
			dsprop = dscount / notecount;
			eprop = ecount / notecount;
			fprop = fcount / notecount;
			fsprop = fscount / notecount;
			gprop = gcount / notecount;
			gsprop = gscount / notecount;

			cout << "Proportion of each note: \n";

			//Output proportion of each note
			cout << fixed << std::setprecision(3) 
				<< "A: " << aprop << "\nA#: " << asprop
				<< "\nB: " << bprop << "\nC: " << cprop
				<< "\nC#: " << csprop << "\nD: " << dprop
				<< "\nD#: " << dsprop << "\nE: " << eprop
				<< "\nF: " << fprop << "\nF#: " << fsprop
				<< "\nG: " << gprop << "\nG#: " << gsprop
				<< "\n\n";


			myfile.close();
			delete[] tempblock;

		}
		else {
			cout << "Input file not open\n";
		}


		do {

			//Output file section



			//Suggested name: "2 Ma 1.txt"
			cout << "Enter a file to output data to: ";

			cin.sync();
			output.clear();
			getline(cin, output);

			output = "Data files\\" + output + ".txt";

			if (!fileExists(output)) {

				cout << "\nThat file doesn't exists, would you like to make a new one? Y or N: ";

				cin.sync();
				cin >> yn;

				yn = toupper(yn);

				//While yn is not a valid character, get yn
				while (yn != 'Y' && yn != 'N') {
					cout << "\nEnter Y or N: ";

					cin.sync();
					cin >> yn;
					yn = toupper(yn);
				}

			}



		} while ((!(fileExists(output)) && yn == 'N'));

		//Open output file
		outfile.open(output.c_str(), ios::app);


		if (outfile.is_open()) {

				do {

					// For thunderstruck, enter 2
					cout << "Enter songs base note, with A being 0, and G# being 11: ";

					cin.sync();
					cin >> basenote;

					// For thunderstruck, enter 1
					cout << "\nEnter major or minor, with major being 1 and minor being 0: ";

					cin.sync();
					cin >> majorminor;

					cout << "\n\n\n";

				} while (!(basenote < 12 && 0 <= basenote && (majorminor == 1 || majorminor == 0)));


			//Key represents the key signature, with 0-11 being all minor keys and 12-23 being all major keys
			key = basenote + (majorminor * 12);

			//Output key value and all note proportions to file
			outfile << fixed << std::setprecision(3) << key << ","
			<< aprop << "," << asprop << ","
			<< bprop << "," << cprop << ","
			<< csprop << "," << dprop << ","
			<< dsprop << "," << eprop << ","
			<< fprop << "," << fsprop << ","
			<< gprop << "," << gsprop << "\n";

			cout << "Data written to file\n\n";

		}
		else {
			cout << "Output file not open\n";
		}

		outfile.close();

		cout << "Would you like to analyze another file? 1 or 0: ";
		cin >> anotherfile;

		cout << "\n";

	} while (anotherfile == '1');

	cout << "Goodbye!\n\n";

    return 0;
}