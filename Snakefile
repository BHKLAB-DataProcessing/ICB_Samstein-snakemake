from snakemake.remote.S3 import RemoteProvider as S3RemoteProvider
S3 = S3RemoteProvider(
    access_key_id=config["key"], 
    secret_access_key=config["secret"],
    host=config["host"],
    stay_on_remote=False
)
prefix = config["prefix"]
filename = config["filename"]
data_source  = "https://raw.githubusercontent.com/BHKLAB-Pachyderm/ICB_Samstein-data/main/"

rule get_MultiAssayExp:
    output:
        S3.remote(prefix + filename)
    input:
        S3.remote(prefix + "processed/CLIN.csv"),
        S3.remote(prefix + "processed/CNA_gene.csv"),
        S3.remote(prefix + "processed/CNA_seg.txt"),
        S3.remote(prefix + "processed/SNV.csv"),
        S3.remote(prefix + "processed/cased_sequenced.csv"),
        S3.remote(prefix + "annotation/Gencode.v40.annotation.RData")
    resources:
        mem_mb=4000
    shell:
        """
        Rscript -e \
        '
        load(paste0("{prefix}", "annotation/Gencode.v40.annotation.RData"))
        source("https://raw.githubusercontent.com/BHKLAB-Pachyderm/ICB_Common/main/code/get_MultiAssayExp.R");
        saveRDS(
            get_MultiAssayExp(study = "Samstein", input_dir = paste0("{prefix}", "processed")), 
            "{prefix}{filename}"
        );
        '
        """

rule download_annotation:
    output:
        S3.remote(prefix + "annotation/Gencode.v40.annotation.RData")
    shell:
        """
        wget https://github.com/BHKLAB-Pachyderm/Annotations/blob/master/Gencode.v40.annotation.RData?raw=true -O {prefix}annotation/Gencode.v40.annotation.RData 
        """

rule format_snv:
    output:
        S3.remote(prefix + "processed/SNV.csv")
    input:
        S3.remote(prefix + "download/SNV.txt.gz"),
        S3.remote(prefix + "processed/cased_sequenced.csv")
    resources:
        mem_mb=2000
    shell:
        """
        Rscript scripts/Format_SNV.R \
        {prefix}download \
        {prefix}processed \
        """

rule format_cna_seg:
    output:
        S3.remote(prefix + "processed/CNA_seg.txt")
    input:
        S3.remote(prefix + "download/CNA_seg.txt.gz"),
        S3.remote(prefix + "download/CLIN_sample.txt"),
        S3.remote(prefix + "processed/cased_sequenced.csv")
    resources:
        mem_mb=2000
    shell:
        """
        Rscript scripts/Format_CNA_seg.R \
        {prefix}download \
        {prefix}processed \
        """

rule format_cna_gene:
    output:
        S3.remote(prefix + "processed/CNA_gene.csv")
    input:
        S3.remote(prefix + "processed/cased_sequenced.csv"),
        S3.remote(prefix + "download/gistic/all_thresholded.by_genes.txt.gz")
    resources:
        mem_mb=2000
    shell:
        """
        Rscript scripts/Format_CNA_gene.R \
        {prefix}download \
        {prefix}processed \
        """

rule format_clin:
    output:
        S3.remote(prefix + "processed/CLIN.csv")
    input:
        S3.remote(prefix + "processed/cased_sequenced.csv"),
        S3.remote(prefix + "download/CLIN.txt"),
        S3.remote(prefix + "download/CLIN_response.txt"),
        S3.remote(prefix + "download/CLIN_sample.txt")
    resources:
        mem_mb=2000
    shell:
        """
        Rscript scripts/Format_CLIN.R \
        {prefix}download \
        {prefix}processed \
        """

rule format_cased_sequenced:
    output:
        S3.remote(prefix + "processed/cased_sequenced.csv")
    input:
        S3.remote(prefix + "download/CLIN.txt")
    resources:
        mem_mb=1000
    shell:
        """
        Rscript scripts/Format_cased_sequenced.R \
        {prefix}download \
        {prefix}processed \
        """

rule download_data:
    output:
        S3.remote(prefix + "download/CLIN.txt"),
        S3.remote(prefix + "download/CLIN_response.txt"),
        S3.remote(prefix + "download/CLIN_sample.txt"),
        S3.remote(prefix + "download/SNV.txt.gz"),
        S3.remote(prefix + "download/gistic/all_thresholded.by_genes.txt.gz"),
        S3.remote(prefix + "download/CNA_seg.txt.gz")
    resources:
        mem_mb=2000
    shell:
        """
        wget {data_source}CLIN.txt -O {prefix}download/CLIN.txt
        wget {data_source}CLIN_response.txt -O {prefix}download/CLIN_response.txt
        wget {data_source}CLIN_sample.txt -O {prefix}download/CLIN_sample.txt
        wget {data_source}SNV.txt.gz -O {prefix}download/SNV.txt.gz
        wget {data_source}gistic/all_thresholded.by_genes.txt.gz -O {prefix}download/gistic/all_thresholded.by_genes.txt.gz
        wget {data_source}CNA_seg.txt.gz -O {prefix}download/CNA_seg.txt.gz
        """ 
