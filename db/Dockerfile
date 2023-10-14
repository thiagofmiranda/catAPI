# Derivando da imagem oficial do MySQL
FROM mysql:5.7

# Adicionando os scripts SQL para serem executados na criação do banco
COPY ./init/ /docker-entrypoint-initdb.d/

RUN chmod -R 775 /docker-entrypoint-initdb.d