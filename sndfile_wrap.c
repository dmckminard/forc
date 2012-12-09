#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sndfile.h>

void free_array(int **data) {
    free(*data);
    return;
}

void read_wav(char *file_name, double **buffer, int *frames, int *sample_rate) {	
    
	// Open sound file
	SF_INFO sndInfo;
	SNDFILE *sndFile = sf_open(file_name, SFM_READ, &sndInfo);
	if (sndFile == NULL) {
		fprintf(stderr, "Error reading source file '%s': %s\n", file_name, sf_strerror(sndFile));
		return;
	}

	// Check format - 16bit PCM
	if (sndInfo.format != (SF_FORMAT_WAV | SF_FORMAT_PCM_16)) {
		fprintf(stderr, "Input should be 16bit Wav\n");
		sf_close(sndFile);
		return;
	}

	// Check channels - mono
	if (sndInfo.channels != 1) {
		fprintf(stderr, "Wrong number of channels\n");
		sf_close(sndFile);
		return;
	}
    
	// Allocate memory
	*buffer = (double *)malloc(sndInfo.frames * sizeof(double));
	if (*buffer == NULL) {
		fprintf(stderr, "Could not allocate memory for file\n");
		sf_close(sndFile);
		return;
	}

	// Load data
	*frames = sf_readf_double(sndFile, (*buffer), sndInfo.frames);
    
	// Check correct number of samples loaded
	if (*frames != sndInfo.frames) {
		fprintf(stderr, "Did not read enough frames for source\n");
		sf_close(sndFile);
		free(*buffer);
		return;
	}

    double size = (double)*frames/sndInfo.samplerate;
    
	// Output Info
	printf("Read %i frames from %s, Sample rate: %d, Length: %fs\n",
		*frames, file_name, sndInfo.samplerate, size);
	
	sf_close(sndFile);

    *sample_rate = sndInfo.samplerate;
    
    printf("\n");
    
    for(int i = 0; i < 5; i++)
        printf("%f\n", (*buffer)[i]);
    
    printf("\n");
    
    return;
    
}

