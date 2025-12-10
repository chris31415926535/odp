# Function to create a list of metadata objects. Must be written with write_metadata().
create_metadata <- function(title = NA,
                            description = NA,
                            contributor = NA,
                            keyword = NA,
                            coverage = NA,
                            identifier = NA,
                            publisher = NA,
                            relation = NA,
                            rights = NA,
                            source = NA,
                            type = NA,
                            subject = NA) {
  metadata <- list()

  if (!is.na(title)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:title",
        `children` = title
      )
    ))
  }

  if (!is.na(description)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:description",
        `children` = description
      )
    ))
  }

  if (!is.na(contributor)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:contributor",
        `children` = contributor
      )
    ))
  }

  if (!is.na(keyword)) {
    metadata <- append(metadata, list(
      list(
        `type` = "meta:keyword",
        `children` = keyword
      )
    ))
  }

  if (!is.na(coverage)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:coverage",
        `children` = coverage
      )
    ))
  }

  if (!is.na(identifier)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:identifier",
        `children` = identifier
      )
    ))
  }

  if (!is.na(publisher)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:publisher",
        `children` = publisher
      )
    ))
  }

  if (!is.na(relation)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:relation",
        `children` = relation
      )
    ))
  }

  if (!is.na(rights)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:rights",
        `children` = rights
      )
    ))
  }

  if (!is.na(source)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:source",
        `children` = source
      )
    ))
  }

  if (!is.na(type)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:type",
        `children` = type
      )
    ))
  }

  if (!is.na(subject)) {
    metadata <- append(metadata, list(
      list(
        `type` = "dc:subject",
        `children` = subject
      )
    ))
  }

  date_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6")

  metadata <- append(metadata, list(
    list(
      `type` = "dc:date",
      `children` = date_time
    )
  ))
  metadata <- append(metadata, list(
    list(
      `type` = "meta:creation-date",
      `children` = date_time
    )
  ))
  metadata <- append(metadata, list(
    list(
      `type` = "meta:print-date",
      `children` = date_time
    )
  ))

  metadata
  list(
    type = "office:meta",
    children = metadata
  )
}


blank_metadata <- function() xml2::read_xml('<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<office:document-meta xmlns:officeooo=\"http://openoffice.org/2009/office\" xmlns:anim=\"urn:oasis:names:tc:opendocument:xmlns:animation:1.0\" xmlns:smil=\"urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0\" xmlns:presentation=\"urn:oasis:names:tc:opendocument:xmlns:presentation:1.0\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" office:version=\"1.4\"/>') # nolint

