package org.chiu.megalith.manage.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:45 am
 */

@Entity
@Table(name ="m_role_menu")
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Setter
@ToString
@DynamicUpdate
public class RoleMenuEntity {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native")
    private Long id;

    @Column(name = "role_id")
    private Long roleId;

    @Column(name = "menu_id")
    private Long menuId;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RoleMenuEntity that = (RoleMenuEntity) o;

        if (!Objects.equals(id, that.id)) return false;
        if (!Objects.equals(roleId, that.roleId)) return false;
        return Objects.equals(menuId, that.menuId);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
