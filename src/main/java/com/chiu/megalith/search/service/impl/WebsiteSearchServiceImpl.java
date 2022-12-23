package com.chiu.megalith.search.service.impl;

import com.chiu.megalith.common.jwt.JwtUtils;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.search.document.WebsiteDocument;
import com.chiu.megalith.search.service.WebsiteSearchService;
import com.chiu.megalith.search.vo.WebsiteVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.stereotype.Service;

import java.time.ZonedDateTime;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
@Service
@RequiredArgsConstructor
public class WebsiteSearchServiceImpl implements WebsiteSearchService {

    private final JwtUtils jwtUtils;

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Override
    public String generateJwt() {
        return jwtUtils.generateToken(Const.ROLE_TOKEN_TOOL.getMsg(), Const.ROLE_TOKEN_TOOL.getMsg());
    }

    @Override
    public void saveOrUpdate(WebsiteVo websiteVo) {
        var ref = new Object() {
            WebsiteDocument document;
        };


        Optional.ofNullable(websiteVo.getId()).ifPresentOrElse(id ->
                ref.document = elasticsearchTemplate.get(id, WebsiteDocument.class), () ->
                ref.document = WebsiteDocument.builder().
                        created(ZonedDateTime.now()).
                        build());

        BeanUtils.copyProperties(websiteVo, ref.document);
        elasticsearchTemplate.save(ref.document);
    }
}
