package com.chiu.megalith.search.vo;

import com.chiu.megalith.base.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.URL;

/**
 * @author mingchiuli
 * @create 2022-12-23 8:10 pm
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WebsiteVo {

    private String id;

    @ListValue(values = {0, 1})
    private Integer status;

    @NotBlank
    private String title;

    @NotBlank
    private String description;

    @URL
    private String link;
}
