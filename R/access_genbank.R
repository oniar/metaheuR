# Hau funtzioa da behar ditugun matrizeak sortzeko
#

seqListToMatrix <- function (sequences, subset) {
  auxFun <- function(i) {
    return(as.character(i[subset]))
  }
  aux <- lapply(sequences, FUN=auxFun)
  return(do.call(rbind, aux))
}


# ADIBIDEAK ---------------------------------------------------------------

# Kanariako mtDNA. Source: http://www.mtdb.igp.uu.se/
genbank.id.list <- c("AF381982", "AF382009", "AF382010", "AY275528","AY275533","AY275537")

seq.example <- ape::read.GenBank(genbank.id.list)

# Hipervariable Regions: HVR1=16024-16383; HVR2=57-372; HVR3=438-574
hvr1 <- seqListToMatrix(seq.example, 16024:16383)
hvr2 <- seqListToMatrix(seq.example, 57:372)
hvr3 <- seqListToMatrix(seq.example, 438:574)

alph<-c('a','c','g','t')

solution.csp.hvr1<-cspGreedy(hvr1,alph)
solution.csp.hvr2<-cspGreedy(hvr2,alph)
solution.csp.hvr3<-cspGreedy(hvr3,alph)
solution.fsp.hvr3<-fspGreedy(hvr3,alph)
solution.fsp.hvr2<-fspGreedy(hvr2,alph)
solution.fsp.hvr1<-fspGreedy(hvr1,alph)

csp.hvr1<-closestStringProblem(hvr1,alph)
csp.hvr2<-closestStringProblem(hvr2,alph)
csp.hvr3<-closestStringProblem(hvr3,alph)
fsp.hvr1<-farthestStringProblem(hvr1,alph)
fsp.hvr2<-farthestStringProblem(hvr2,alph)
fsp.hvr3<-farthestStringProblem(hvr3,alph)
