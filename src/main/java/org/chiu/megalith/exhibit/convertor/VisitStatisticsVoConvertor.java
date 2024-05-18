package org.chiu.megalith.exhibit.convertor;

import org.chiu.megalith.exhibit.vo.VisitStatisticsVo;

import java.util.List;

public class VisitStatisticsVoConvertor {

    private VisitStatisticsVoConvertor() {}

    public static VisitStatisticsVo convert(List<Long> items) {
        return VisitStatisticsVo.builder()
                .dayVisit(items.get(0))
                .weekVisit(items.get(1))
                .monthVisit(items.get(2))
                .yearVisit(items.get(3))
                .build();
    }
}
