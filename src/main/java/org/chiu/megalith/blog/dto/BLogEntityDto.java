package org.chiu.megalith.blog.dto;

import lombok.Data;

import java.io.Serializable;

@Data
public class BLogEntityDto implements Serializable {

    private Long id;

    private String title;

    private String description;

    private String content;

    private Integer status;

    private String link;
}
