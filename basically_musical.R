library("dplyr")
library("audio")

# assign each nucleotide to a musical note
nucleotide_note = c("G" = "Re", "C" = "Mi", "T" = "Sol", "A" = "La")


# assign frequency to each musical note
note_frequency = c("Re" = 293.6648, "Mi" = 329.6276, "Sol" = 391.9954, "La" = 440.0000)



# a random DNA sequence
DNA_sequence <- "ATGCATGCATCGATGCATGGATCGATGCATGCATGCGCGTAGCTAGCGTAGCTAGCTAGCTGATGCTAGATGCATGCATGCATGCATGCATGCATGCATGCATGC"


# assign the corresponding frequencies for each nucleotides
frequency_sequence = c()

for (i in strsplit(DNA_sequence, "")) {
  frequency_sequence = append(frequency_sequence, note_frequency[nucleotide_note[i]])
  
}


# assign tempo and sample rate for the sound waves
tempo <- 140
sample_rate <- 44100


# A function to return the an object of sine wave corresponding to a frequence
make_sine <- function(freqency) {
  wave <- sin(seq(0, 1 / tempo * 60, 1 / sample_rate) *
                freqency * 2 * pi) 
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}


DNA_wave <-
  mapply(make_sine, frequency_sequence) 

play(DNA_wave)


